@echo off
echo Fixing duplicate kpictureeditor.dfm in KControls 1.7.3 (Online Package Manager)...
set "KC_DIR=%LOCALAPPDATA%\lazarus\onlinepackagemanager\packages\KControls"
if exist "%KC_DIR%\packages\lazarus\lib\x86_64-win64\kpictureeditor.dfm" (
    del /f /q "%KC_DIR%\packages\lazarus\lib\x86_64-win64\kpictureeditor.dfm"
    echo Removed duplicate "%KC_DIR%\packages\lazarus\lib\x86_64-win64\kpictureeditor.dfm"
) else (
    echo No duplicate kpictureeditor.dfm found in "%KC_DIR%\packages\lazarus\lib\x86_64-win64\"
)
echo Done!
pause
