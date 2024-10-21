cd %~dp0
cd ..

@echo "Remove old files"
del /q /s /f *.wixobj
del /q /s /f *.wixpdb
del /q /s /f ReleaseFiles*
@echo "Generating msi"
candle wix\sonic-pi.wxs etc.wxs gui.wxs server.wxs config.wxs -ext WixUtilExtension -arch x64
light sonic-pi.wixobj etc.wixobj gui.wixobj server.wixobj config.wixobj -ext WixUtilExtension -ext WixUIExtension -o wix\Sonic-Pi.msi -b etc -b app\gui -b app\server -b app\config
