cd %~dp0
cd ..
@echo "Generating msi"
candle wix\sonic-pi.wxs etc.wxs gui.wxs server.wxs -ext WixUtilExtension -arch x64
light sonic-pi.wixobj etc.wixobj gui.wixobj server.wixobj -ext WixUtilExtension -ext WixUIExtension -o wix\Sonic-Pi.msi -b etc -b app\gui -b app\server
