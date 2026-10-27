# FPC OpenSSL compatibility package

Upstream: https://gitlab.com/freepascal.org/fpc/source, fixes_3_2.
The exact commit, upstream URLs and source hashes are in provenance.json.

These project-local units add OpenSSL 3 loading, the SSL_get1_peer_certificate
fallback and SSL_set1_host hostname verification while retaining FPC 3.2.2.
Put this directory before compiler-provided OpenSSL units in the unit search path.
Use a clean unit output directory after changing this package.

The local compatibility patch removes `override` from the two
GetLastSSLError* declarations in opensslsockets.pp because the installed
FPC 3.2.2 TSSLSocketHandler does not yet declare those virtual methods.
It also retains file-only CA loading because FPC 3.2.2 has no TrustedCertsDir.
The explicit bundled CA and upstream hostname verification remain enabled.
The resulting source hash is recorded separately in provenance.json.

openssl.pas retains its BSD license header. fpopenssl.pp refers to COPYING.FPC;
the FPC runtime uses the modified LGPL linking exception. Include these notices
with distributed binaries and preserve source headers in source distributions.
