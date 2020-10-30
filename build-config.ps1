echo $PWD
cd app\gui\qt
echo $PWD
dir
cmd /c win-prebuild.bat
cmd /c win-config.bat
dir
cd ..\..\..