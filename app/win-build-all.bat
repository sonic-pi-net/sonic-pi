cd %~dp0

call win-prebuild.bat
call win-config.bat

cd build
cmake --build . --config Release

cd %~dp0
