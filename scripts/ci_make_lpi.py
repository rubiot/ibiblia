#!/usr/bin/env python3
"""Write a copy of an .lpi without the given required packages (CI only).

usage: ci_make_lpi.py <in.lpi> <out.lpi> <PackageName> [<PackageName> ...]

The Windows project lists the optional "lazrichview" package, which is not
available in CI and is not used by the code. The project file in the repository
is left untouched.
"""
import sys
import xml.etree.ElementTree as ET

src, dst, drop = sys.argv[1], sys.argv[2], set(sys.argv[3:])
tree = ET.parse(src)
for req in tree.getroot().iter("RequiredPackages"):
    items = [i for i in list(req) if i.tag.startswith("Item")]
    keep = [i for i in items
            if i.find("PackageName") is None or i.find("PackageName").get("Value") not in drop]
    for i in items:
        req.remove(i)
    for n, i in enumerate(keep, 1):
        i.tag = "Item%d" % n
        req.append(i)
    req.set("Count", str(len(keep)))
tree.write(dst, encoding="UTF-8", xml_declaration=True)
