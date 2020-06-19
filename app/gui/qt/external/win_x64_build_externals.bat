cd %~dp0
mkdir build > nul
cd build
cmake -G "Visual Studio 16 2019" -A x64 -D ERLANG_INCLUDE_PATH=%~dp0..\..\..\..\prebuilt\windows\headers\erlang\ ..\
cmake --build . --config Release
cd %CURRENT_DIR%
