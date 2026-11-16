#!/usr/bin/env python3
"""Print the CHANGELOG.md section for one version (used as the release text).

usage: ci_release_notes.py <version like 1.1.0.027>
"""
import re
import sys

ver = sys.argv[1]
text = open("CHANGELOG.md", encoding="utf-8").read().replace("\r\n", "\n")
# sections look like "### 0. Version 1.1.0.027 (date) — title" or "### 0e. Version ..."
heads = list(re.finditer(r"^### [^\n]*Version (\d+(?:\.\d+)+)[^\n]*$", text, re.M))
for n, m in enumerate(heads):
    if m.group(1) == ver:
        end = heads[n + 1].start() if n + 1 < len(heads) else len(text)
        body = text[m.end():end].strip()
        print(body)
        break
else:
    print("iBiblia %s" % ver)
