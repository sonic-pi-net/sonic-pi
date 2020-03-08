REM Pass 'Release' or 'Debug' on the command line
REM
@echo Remember to update version number in wix\sonic-pi.wxs!!

cd %~dp0
call copy_files.bat

REM Now we have etc/app folders, generate the installer from them
del gui.wix
del etc.wix
del *.wixobj
call wix\gen_wix.bat
call wix\gen_msi_x86.bat
