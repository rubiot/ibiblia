#!/usr/bin/env python3
"""Fail unless both project files carry the version named by the release tag.

usage: ci_check_version.py <tag like v1.1.0.027>   (prints the plain version)
"""
import re
import sys
import xml.etree.ElementTree as ET

tag = sys.argv[1]
want = tuple(int(x) for x in re.findall(r"\d+", tag)[:4])
if len(want) != 4:
    raise SystemExit("tag %r is not a 4-part version" % tag)
for lpi in ("iBiblia.lpi", "iBiblia-linux.lpi"):
    vi = ET.parse(lpi).getroot().find(".//VersionInfo")
    got = tuple(int(vi.find(k).get("Value", "0")) if vi.find(k) is not None else 0
                for k in ("MajorVersionNr", "MinorVersionNr", "RevisionNr", "BuildNr"))
    if got != want:
        raise SystemExit("%s says %s but the tag says %s" % (lpi, ".".join(map(str, got)), ".".join(map(str, want))))
print("%d.%d.%d.%03d" % want)
