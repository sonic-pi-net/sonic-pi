REM Copy just the parts we need.  After copying it is possible to try the exe and make sure everything works
REM Please add commands to delete or specialize the copy more to remove extra things that shouldn't be in the build
REM /Y Means just do it, don't ask
REM /R Recursive
REM /E Include empty folders (so that children get included too!)
REM /I Ensures directories get copied properly

xcopy /Y /I /R /E ..\..\app\gui\qt\Release app\gui\qt\Release
xcopy /Y /I /R /E ..\..\etc etc\

xcopy /Y /I /R /E ..\..\app\server\erlang app\server\erlang

xcopy /Y /I /R /E ..\..\app\server\native\osmid app\server\native\osmid
xcopy /Y /I /R /E ..\..\app\server\native\plugins app\server\native\plugins
xcopy /Y /I /R /E ..\..\app\server\native\ruby\bin app\server\native\ruby\bin
xcopy /Y /I /R /E ..\..\app\server\native\ruby\lib app\server\native\ruby\lib
xcopy /Y /I /R /E ..\..\app\server\native\ruby\share app\server\native\ruby\share
xcopy /Y /I /R /E ..\..\app\server\native\ruby\ssl app\server\native\ruby\ssl
xcopy /Y ..\..\app\server\native\*.* app\server\native

xcopy /Y /I /R /E ..\app\server\ruby app\server\ruby

REM Now we have etc/app folders, generate the installer from them
call wix\gen_wix.bat
call wix\gen_msi.bat
