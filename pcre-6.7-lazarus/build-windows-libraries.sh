# This script builds the PCRE library for Windows using MinGW
# It creates both 32-bit and 64-bit versions of the library
# Ensure you have MinGW installed first (sudo apt install mingw-w64)
# Usage: bash ./build-windows-libraries.sh

# Check if MinGW is installed
if ! command -v x86_64-w64-mingw32-gcc &> /dev/null
then
    echo "MinGW is not installed. Please install it first (apt install mingw-w64)."
    exit
fi
if ! command -v i686-w64-mingw32-gcc &> /dev/null
then
    echo "MinGW is not installed. Please install it first (apt install mingw-w64)."
    exit
fi
# Check if the script is run from the correct directory
if [ ! -f "pcre_compile.c" ]; then
    echo "Please run this script from the directory containing the PCRE source files."
    exit
fi
# Build 32-bit version
echo "Building 32-bit version..."
i686-w64-mingw32-gcc -O2 -shared -o pcre32.dll pcre_compile.c pcre_config.c pcre_dfa_exec.c pcre_exec.c pcre_fullinfo.c pcre_get.c pcre_globals.c pcre_info.c pcre_maketables.c pcre_ord2utf8.c pcre_refcount.c pcre_study.c pcre_tables.c pcre_try_flipped.c pcre_ucp_searchfuncs.c pcre_valid_utf8.c pcre_version.c pcre_xclass.c pcreposix.c pcre_helpers.c pcre_chartables.c -I. -Wl,--out-implib,libpcre32.a
# Check if the build was successful
if [ $? -ne 0 ]; then
    echo "32-bit build failed."
    exit 1
fi
# Build 64-bit version
echo "Building 64-bit version..."
x86_64-w64-mingw32-gcc -O2 -shared -o pcre64.dll pcre_compile.c pcre_config.c pcre_dfa_exec.c pcre_exec.c pcre_fullinfo.c pcre_get.c pcre_globals.c pcre_info.c pcre_maketables.c pcre_ord2utf8.c pcre_refcount.c pcre_study.c pcre_tables.c pcre_try_flipped.c pcre_ucp_searchfuncs.c pcre_valid_utf8.c pcre_version.c pcre_xclass.c pcreposix.c pcre_helpers.c pcre_chartables.c -I. -Wl,--out-implib,libpcre64.a
# Check if the build was successful
if [ $? -ne 0 ]; then
    echo "64-bit build failed."
    exit 1
fi

echo "Build completed successfully."
echo "32-bit library: pcre32.dll, libpcre32.a"
echo "64-bit library: pcre64.dll, libpcre64.a"
echo "You can now use these libraries in your Lazarus projects."