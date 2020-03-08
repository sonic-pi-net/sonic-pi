cd %~dp0
cd ..
@echo "Generating msi"
candle wix\sonic-pi-x86.wxs etc.wxs gui.wxs server.wxs -ext WixUtilExtension
light sonic-pi-x86.wixobj etc.wixobj gui.wixobj server.wixobj -ext WixUtilExtension -ext WixUIExtension -o wix\Sonic-Pi.msi -b etc -b app\gui -b app\server
