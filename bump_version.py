#!/usr/bin/env python3
"""
iBiblia Version Incrementor Script
Follows the 1.0.1.xxx numbering convention starting at 1.0.1.000 with .001 increments.
"""

import sys
import re
import os

def bump_version(target_dir='.'):
    lpi_path = os.path.join(target_dir, 'iBiblia.lpi')
    about_path = os.path.join(target_dir, 'unitabout.pas')
    hist_path = os.path.join(target_dir, 'Histórico.txt')
    hist_lower_path = os.path.join(target_dir, 'historico.txt')

    if not os.path.exists(lpi_path):
        print(f"Error: {lpi_path} not found.")
        return False

    with open(lpi_path, 'r', encoding='utf-8') as f:
        lpi_content = f.read()

    # Find current FileVersion
    m = re.search(r'FileVersion="1\.0\.1\.(\d{3})"', lpi_content)
    if not m:
        print("Current version is not in 1.0.1.xxx format. Initializing to 1.0.1.000.")
        current_build = -1
    else:
        current_build = int(m.group(1))

    next_build = current_build + 1
    new_ver_str = f"1.0.1.{next_build:03d}"
    print(f"Bumping version from 1.0.1.{max(0, current_build):03d} to {new_ver_str}...")

    # 1. Update iBiblia.lpi
    lpi_content = re.sub(r'<MajorVersionNr Value="\d+"/>', '<MajorVersionNr Value="1"/>', lpi_content)
    lpi_content = re.sub(r'<MinorVersionNr Value="\d+"/>', '<MinorVersionNr Value="0"/>', lpi_content)
    lpi_content = re.sub(r'<RevisionNr Value="\d+"/>', '<RevisionNr Value="1"/>', lpi_content)
    lpi_content = re.sub(r'<BuildNr Value="\d+"/>', f'<BuildNr Value="{next_build}"/>', lpi_content)
    lpi_content = re.sub(r'FileVersion="[0-9.]+"', f'FileVersion="{new_ver_str}"', lpi_content)
    lpi_content = re.sub(r'ProductVersion="[0-9.]+"', f'ProductVersion="{new_ver_str}"', lpi_content)

    with open(lpi_path, 'w', encoding='utf-8') as f:
        f.write(lpi_content)

    # 2. Update unitabout.pas
    if os.path.exists(about_path):
        with open(about_path, 'r', encoding='utf-8') as f:
            about_content = f.read()
        about_content = re.sub(r'iBiblia Version [0-9.]+ \(64-bit Edition\)', f'iBiblia Version {new_ver_str} (64-bit Edition)', about_content)
        about_content = re.sub(r'NEW FEATURES & UPDATES \(v[0-9.]+\):', f'NEW FEATURES & UPDATES (v{new_ver_str}):', about_content)
        about_content = re.sub(r"ver := '[0-9.]+';", f"ver := '{new_ver_str}';", about_content)
        about_content = re.sub(r"lblTitle\.Caption := 'iBiblia v[0-9.]+ - Changelog';", f"lblTitle.Caption := 'iBiblia v{new_ver_str} - Changelog';", about_content)
        with open(about_path, 'w', encoding='utf-8') as f:
            f.write(about_content)

    print(f"Successfully bumped project files to Version {new_ver_str} (64-bit Edition).")
    return True

if __name__ == '__main__':
    target = sys.argv[1] if len(sys.argv) > 1 else 'src/ibiblia-master'
    bump_version(target)