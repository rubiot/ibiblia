"""Exercise the packaged Pascal HTTPS client without API keys or public services."""
import argparse
import json
import shutil
import socket
import ssl
import subprocess
import tempfile
import threading
from pathlib import Path

DLLS = ("libssl-3-x64.dll", "libcrypto-3-x64.dll")


def generate_certificates(openssl, directory):
    def command(*arguments):
        result = subprocess.run([str(openssl), *arguments], cwd=directory,
                                capture_output=True, text=True)
        if result.returncode:
            raise RuntimeError("OpenSSL test fixture failed: " + result.stderr)

    (directory / "test-openssl.cnf").write_text(
        "[req]\ndistinguished_name=req_dn\n[req_dn]\n", encoding="ascii")
    command("req", "-config", "test-openssl.cnf", "-x509", "-newkey", "rsa:2048", "-nodes", "-days", "2",
            "-subj", "/CN=iBiblia disposable TLS test CA", "-keyout", "ca.key",
            "-out", "ca.pem", "-addext", "basicConstraints=critical,CA:TRUE",
            "-addext", "keyUsage=critical,keyCertSign,cRLSign")
    command("req", "-config", "test-openssl.cnf", "-new", "-newkey", "rsa:2048", "-nodes", "-subj",
            "/CN=localhost", "-keyout", "server.key", "-out", "server.csr")
    (directory / "server.ext").write_text(
        "subjectAltName=DNS:localhost\nbasicConstraints=critical,CA:FALSE\n"
        "keyUsage=critical,digitalSignature,keyEncipherment\n"
        "extendedKeyUsage=serverAuth\n", encoding="ascii")
    command("x509", "-req", "-in", "server.csr", "-CA", "ca.pem", "-CAkey",
            "ca.key", "-CAcreateserial", "-days", "2", "-out", "server.pem",
            "-extfile", "server.ext")
    (directory / "wrong-server.ext").write_text(
        (directory / "server.ext").read_text().replace(
            "DNS:localhost", "DNS:unrelated.example"), encoding="ascii")
    command("x509", "-req", "-in", "server.csr", "-CA", "ca.pem", "-CAkey",
            "ca.key", "-CAcreateserial", "-days", "2", "-out", "wrong-server.pem",
            "-extfile", "wrong-server.ext")


def make_runtime(source, destination, ca_file):
    destination.mkdir()
    for filename in DLLS:
        shutil.copy2(source / filename, destination / filename)
    shutil.copy2(ca_file, destination / "cacert.pem")


def run_https_probe(probe, runtime, certificates, hostname, expect_rejection,
                    certificate="server.pem"):
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    context.minimum_version = ssl.TLSVersion.TLSv1_2
    context.load_cert_chain(certificates / certificate, certificates / "server.key")
    listener = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    listener.bind(("127.0.0.1", 0))
    listener.listen(1)
    listener.settimeout(15)
    port = listener.getsockname()[1]
    unexpected_errors = []

    def serve():
        try:
            connection, _ = listener.accept()
            connection.settimeout(10)
            with connection:
                try:
                    with context.wrap_socket(connection, server_side=True) as secured:
                        secured.recv(4096)
                        secured.sendall(b"HTTP/1.1 200 OK\r\nContent-Length: 2\r\n"
                                        b"Connection: close\r\n\r\nOK")
                except ssl.SSLError:
                    if not expect_rejection:
                        raise
        except Exception as error:
            unexpected_errors.append(repr(error))
        finally:
            listener.close()

    server = threading.Thread(target=serve, daemon=True)
    server.start()
    arguments = [str(probe), "--runtime-dir", str(runtime), "--url",
                 f"https://{hostname}:{port}/"]
    if expect_rejection:
        arguments.append("--expect-certificate-error")
    process = subprocess.run(arguments, capture_output=True, text=True, timeout=20)
    server.join(timeout=16)
    expected_marker = "TLS_REJECTED verification=" if expect_rejection else "TLS_OK status=200"
    if process.returncode or expected_marker not in process.stdout or unexpected_errors:
        raise AssertionError(json.dumps({"host": hostname, "exit_code": process.returncode,
                                        "stdout": process.stdout, "stderr": process.stderr,
                                        "server_errors": unexpected_errors}))
    return process.stdout.strip()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--probe", type=Path, required=True)
    parser.add_argument("--runtime-dir", type=Path, required=True)
    parser.add_argument("--openssl", type=Path, required=True)
    args = parser.parse_args()
    probe, runtime, openssl = (args.probe.resolve(), args.runtime_dir.resolve(),
                               args.openssl.resolve())
    for filename in [probe, openssl, runtime / "cacert.pem", *(runtime / f for f in DLLS)]:
        if not filename.is_file():
            raise FileNotFoundError(filename)
    with tempfile.TemporaryDirectory(prefix="ibiblia TLS café-") as temporary:
        workspace = Path(temporary)
        certificates = workspace / "certificates"
        certificates.mkdir()
        generate_certificates(openssl, certificates)
        trusted = workspace / "trusted-runtime"
        untrusted = workspace / "untrusted-runtime"
        make_runtime(runtime, trusted, certificates / "ca.pem")
        make_runtime(runtime, untrusted, runtime / "cacert.pem")
        results = {
            "trusted_localhost": run_https_probe(probe, trusted, certificates, "localhost", False),
            "wrong_hostname": run_https_probe(probe, trusted, certificates, "localhost", True,
                                              "wrong-server.pem"),
            "wrong_ip_identity": run_https_probe(probe, trusted, certificates, "127.0.0.1", True),
            "untrusted_chain": run_https_probe(probe, untrusted, certificates, "localhost", True),
        }
        for label, missing_file in (("missing_libraries", DLLS[0]), ("missing_ca", "cacert.pem")):
            missing_runtime = workspace / label
            missing_runtime.mkdir()
            if missing_file == "cacert.pem":
                for filename in DLLS:
                    shutil.copy2(runtime / filename, missing_runtime / filename)
            process = subprocess.run([str(probe), "--runtime-dir", str(missing_runtime),
                                      "--check-runtime"], capture_output=True, text=True, timeout=10)
            if process.returncode == 0 or missing_file not in process.stderr:
                raise AssertionError(label + ": " + process.stdout + process.stderr)
            results[label] = "TLS_RUNTIME_REJECTED " + missing_file
        print(json.dumps({"passed": len(results), "results": results}, indent=2))


if __name__ == "__main__":
    main()
