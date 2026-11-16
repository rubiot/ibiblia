iBiblia 1.1.0.026 for Linux (64-bit)
====================================

Built for Debian 12 (bookworm) and Debian 13 (trixie) on 64-bit PCs
(x86_64). It also runs on Ubuntu and other Debian-based systems.


How to run it
-------------
1. Extract this folder anywhere in your home folder, for example
   ~/Apps/iBiblia. Keep all the files in it together.
2. Open the folder and double-click "iBiblia".
   If double-clicking does not start it, open a terminal in the folder
   and type:   ./iBiblia
3. Optional: to add iBiblia to your applications menu, open a terminal
   in the folder and type:   ./install-menu-entry.sh
   (Run it again if you ever move the folder.)


If it does not start
--------------------
It needs the GTK2 and SQLite libraries. Most Debian desktops already
have them. If not, open a terminal and type:

   Debian 12:  sudo apt install libgtk2.0-0 libsqlite3-0
   Debian 13:  sudo apt install libgtk2.0-0t64 libsqlite3-0

The PCRE library iBiblia needs (libpcre.so.3) is included in this
folder, because Debian 13 no longer ships it.


Optional features
-----------------
- Reading words aloud (Tools > Voice settings) needs a speech engine:
     sudo apt install espeak-ng
- The AI-assisted engine uses the system OpenSSL (libssl3) and
  certificates (ca-certificates), which Debian installs by default.
- Synchronization with theWord and Timotheos is Windows-only and is
  hidden in this version.


Your settings and projects
--------------------------
- Your projects are the same .bib files you use on Windows. You can
  open them here and take them back to Windows.
- Settings are saved in iBiblia.ini in this folder. If the folder is
  read-only, they go to ~/.config/iBiblia instead.
- To keep your Windows settings, copy your iBiblia.ini from the
  Windows iBiblia folder into this folder. Dictionary paths in it
  point to Windows folders, so choose your dictionaries again in
  Tools > Default dictionaries.


Good to know
------------
- Synchronising with theWord is meant for Windows; theWord is a Windows
  program. It was not tested on Linux.
- The skins work. Buttons and drop-down lists keep your desktop's
  own look, because Linux draws those itself.
- Read aloud (text to speech) uses the Windows voices, so it is not
  available on Linux.


Licences
--------
iBiblia is free software. Third-party notices are in
THIRD-PARTY-NOTICES.txt and the licenses folder (PCRE, SQLite, OpenSSL,
Mozilla CA list).
