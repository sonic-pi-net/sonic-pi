# https://github.com/actions/runner/issues/382#issuecomment-602055936
$ErrorView = "NormalView"

cd app\gui\qt
cmd /c win-prebuild.bat
cmd /c win-config.bat
cd ..\..\..