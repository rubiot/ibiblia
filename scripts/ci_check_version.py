#!/usr/bin/env python3
"""Print the version in the project files; fail if they disagree.

usage: ci_check_version.py [tag like v1.1.0.027]
  Without a tag it just reports the version (iBiblia.lpi and, when present,
  iBiblia-linux.lpi must agree). With a tag the version must also match it.
"""
import os
import re
import sys
import xml.etree.ElementTree as ET


def version_of(lpi):
    vi = ET.parse(lpi).getroot().find(".//VersionInfo")
    return tuple(int(vi.find(k).get("Value", "0")) if vi.find(k) is not None else 0
                 for k in ("MajorVersionNr", "MinorVersionNr", "RevisionNr", "BuildNr"))


found = {lpi: version_of(lpi) for lpi in ("iBiblia.lpi", "iBiblia-linux.lpi") if os.path.exists(lpi)}
want = next(iter(found.values()))
for lpi, got in found.items():
    if got != want:
        raise SystemExit("%s says %s but iBiblia.lpi says %s" % (lpi, got, want))
if len(sys.argv) > 1:
    tag = tuple(int(x) for x in re.findall(r"\d+", sys.argv[1])[:4])
    if tag != want:
        raise SystemExit("tag %s does not match the project version %s" % (sys.argv[1], want))
print("%d.%d.%d.%03d" % want)
