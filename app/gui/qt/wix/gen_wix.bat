cd %~dp0
cd ..

heat dir etc -gg -g1 -sfrag -dr SonicPi -cg ETC -out etc.wxs
heat dir app\gui -gg -g1 -sfrag -dr AppDir -cg GUI -out gui.wxs
heat dir app\server -gg -g1 -sfrag -dr AppDir -cg SERVER -out server.wxs

echo Update version number in wix\sonic-pi.wxs
echo Remove duplicate sonic-pi.exe info from wix\gui.wxs
echo Then run gen_msi.bat
