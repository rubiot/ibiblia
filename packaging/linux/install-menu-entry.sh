#!/bin/sh
# Adds iBiblia to your applications menu (for your user only).
# Run it again if you move this folder.
HERE=$(cd "$(dirname "$0")" && pwd)
mkdir -p "$HOME/.local/share/applications"
cat > "$HOME/.local/share/applications/ibiblia.desktop" <<EOF
[Desktop Entry]
Type=Application
Name=iBiblia
Comment=Associate Strong's numbers and morphology between Bible texts
Exec="$HERE/iBiblia" %f
Icon=$HERE/iBiblia.png
Terminal=false
Categories=Education;Office;
MimeType=application/x-ibiblia-project;
EOF
chmod +x "$HOME/.local/share/applications/ibiblia.desktop"
echo "iBiblia was added to your applications menu."
