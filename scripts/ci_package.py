#!/usr/bin/env python3
"""Build the release ZIP for one platform (used by the GitHub workflow).

usage: ci_package.py windows|linux <version> <build_dir> <out_dir> [--libpcre PATH]

  windows  build_dir holds iBiblia.exe; runtime DLLs/CA bundle are fetched
           according to release/dependencies.lock.json and checked against the
           SHA-256 values pinned there.
  linux    build_dir holds the iBiblia binary; libpcre.so.3 comes from --libpcre.

Output: <out_dir>/iBiblia-<version>-<platform>.zip
The app updater looks for the "-windows-x64.zip" / "-linux-x86_64.zip" suffix
and checks the file against SHA256SUMS.txt (written by the workflow).
"""
import hashlib
import io
import json
import os
import shutil
import stat
import sys
import urllib.request
import zipfile

REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))


def sha256(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def fetch(url, dest):
    req = urllib.request.Request(url, headers={"User-Agent": "iBiblia-release"})
    with urllib.request.urlopen(req, timeout=120) as r, open(dest, "wb") as f:
        shutil.copyfileobj(r, f)


class Payload:
    """Files to put in the ZIP: (archive name, source path, executable?)."""

    def __init__(self):
        self.items = {}

    def add(self, arcname, src, exe=False):
        if not os.path.isfile(src):
            raise SystemExit("missing file: %s" % src)
        self.items[arcname.replace("\\", "/")] = (src, exe)

    def add_tree(self, arcdir, srcdir, patterns=None):
        for root, _dirs, files in os.walk(srcdir):
            for name in sorted(files):
                if patterns and not any(name.endswith(p) for p in patterns):
                    continue
                full = os.path.join(root, name)
                rel = os.path.relpath(full, srcdir)
                self.add(arcdir + "/" + rel if arcdir else rel, full)

    def write(self, zip_path):
        os.makedirs(os.path.dirname(zip_path), exist_ok=True)
        with zipfile.ZipFile(zip_path, "w", zipfile.ZIP_DEFLATED) as z:
            for arc in sorted(self.items):
                src, exe = self.items[arc]
                zi = zipfile.ZipInfo(arc, date_time=(2026, 1, 1, 0, 0, 0))
                zi.compress_type = zipfile.ZIP_DEFLATED
                mode = 0o755 if exe else 0o644
                zi.external_attr = (stat.S_IFREG | mode) << 16
                with open(src, "rb") as f:
                    z.writestr(zi, f.read())


def build_windows(version, build_dir, out_dir):
    lock = json.load(open(os.path.join(REPO, "release", "dependencies.lock.json"), encoding="utf-8"))
    work = os.path.join(out_dir, "_deps")
    os.makedirs(work, exist_ok=True)
    runtime = {}  # destination -> path
    for dep in lock["dependencies"]:
        if dep["kind"] == "repository":
            root = REPO
        else:
            archive = os.path.join(work, dep["archiveName"])
            if not os.path.exists(archive):
                fetch(dep["url"], archive)
            if sha256(archive) != dep["sha256"]:
                raise SystemExit("checksum mismatch for %s" % dep["id"])
            if dep["kind"] == "zip":
                root = os.path.join(work, dep["id"])
                with zipfile.ZipFile(archive) as z:
                    z.extractall(root)
            else:
                root = work
        for f in dep["files"]:
            if f["destination"].startswith(".release-tools"):
                continue
            src = os.path.join(root, f["source"])
            if sha256(src) != f["sha256"]:
                raise SystemExit("checksum mismatch for %s" % f["source"])
            runtime[f["destination"]] = src
    p = Payload()
    for item in lock["payload"]:
        dest, source = item["destination"], item["source"]
        if source == "iBiblia.exe":
            p.add(dest, os.path.join(build_dir, "iBiblia.exe"), exe=True)
        elif dest in runtime:
            p.add(dest, runtime[dest])
        else:
            p.add(dest, os.path.join(REPO, source))
    return p


def build_linux(version, build_dir, libpcre, out_dir):
    p = Payload()
    p.add("iBiblia", os.path.join(build_dir, "iBiblia"), exe=True)
    p.add("libpcre.so.3", libpcre)
    p.add("lemmas.dat", os.path.join(REPO, "lemmas.dat"))
    p.add("iBiblia.models.json", os.path.join(REPO, "iBiblia.models.json"))
    p.add("historico.txt", os.path.join(REPO, "historico.txt"))
    p.add_tree("languages", os.path.join(REPO, "languages"), (".po", ".mo"))
    lin = os.path.join(REPO, "packaging", "linux")
    for name in ("THIRD-PARTY-NOTICES.txt", "iBiblia.png"):
        p.add(name, os.path.join(lin, name))
    p.add("install-menu-entry.sh", os.path.join(lin, "install-menu-entry.sh"), exe=True)
    p.add_tree("licenses", os.path.join(lin, "licenses"))
    # README carries the version number
    readme = open(os.path.join(lin, "README-Linux.txt"), encoding="utf-8").read()
    readme = readme.replace("1.1.0.026", version)
    os.makedirs(out_dir, exist_ok=True)
    tmp = os.path.join(out_dir, "README-Linux.generated.txt")
    open(tmp, "w", encoding="utf-8").write(readme)
    p.add("README-Linux.txt", tmp)
    return p


def main():
    if len(sys.argv) < 5:
        raise SystemExit(__doc__)
    platform, version, build_dir, out_dir = sys.argv[1:5]
    libpcre = sys.argv[sys.argv.index("--libpcre") + 1] if "--libpcre" in sys.argv else None
    if platform == "windows":
        payload, suffix = build_windows(version, build_dir, out_dir), "windows-x64"
    elif platform == "linux":
        payload, suffix = build_linux(version, build_dir, libpcre, out_dir), "linux-x86_64"
    else:
        raise SystemExit("unknown platform")
    zip_path = os.path.join(out_dir, "iBiblia-%s-%s.zip" % (version, suffix))
    payload.write(zip_path)
    print("wrote", zip_path, os.path.getsize(zip_path), "bytes,", len(payload.items), "files")


if __name__ == "__main__":
    main()
