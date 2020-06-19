cd %~dp0
mkdir build_x86 > nul
cd build_x86
cmake -G "Visual Studio 16 2019" -A Win32 -D ERLANG_INCLUDE_PATH=%~dp0..\..\..\..\prebuilt\windows\headers\erlang\ ..\
cmake --build . --config Release
cd %CURRENT_DIR%
