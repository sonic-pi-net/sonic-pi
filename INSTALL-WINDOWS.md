# Building the Latest Sonic Pi on Windows #

Updated: 31/1/2020
Please let us know if you have any issues following these instructions.  
The current build on windows is assumed to be a 32 or 64 bit build done with Visual Studio 2019 (Community edition is fine).
It is recommended that you clone the whole tree again if you want to make a 32 or 64 bit build.

Start by cloning the Sonic Pi Repository in the usual way

```
git clone https://github.com/samaaron/sonic-pi.git c:/dev/sonic-pi
``` 

## Build the UI

1) Install Qt.  Qt is not something you want to try building on windows; it is much better to install - be sure to pick the 5.14.1 checkbox to get the installed libraries, and pick the 64 bit or 32 bit or both options for msvc.  Note that this not on by default!
After install, setup an environment variable to point to the install location.  I like to use Rapid Environment Editor to setup these variables permanently (https://www.rapidee.com/en/about).  Otherwise the setx command can make global variables, but the command line needs to be restarted afterwards.  The current recommended version is 5.14.1, but other recent versions should work.
The 32 bit variant of the variable is only needed for 32 bit builds
```
http://download.qt.io/official_releases/qt/5.14/5.14.1/qt-opensource-windows-x86-5.14.1.exe
setx QT_INSTALL_LOCATION C:\Qt\Qt5.14.1\5.14.1\msvc2017_64 (restart command prompt)
setx QT_INSTALL_LOCATION32 C:\Qt\Qt5.14.1\5.14.1\msvc2017 (restart command prompt)
```

2) Install the latest CMake http://www.cmake.org/download.  This is a build tool that is required

3) Some ruby is needed to run the translations and generate the help headers before the main build; so install the gems.
Install Ruby from http://rubyinstaller.org/downloads. Get the version with the devkit, 64 or 32 bit, depending on your platform.  You should update aubio to 3.3 if you have an older version.
```
https://github.com/oneclick/rubyinstaller2/releases/download/RubyInstaller-2.6.5-1/rubyinstaller-devkit-2.6.5-1-x64.exe

gem install win32-process
gem install rugged --version 0.27.1
gem install ffi
```
Run a console as administrator. Add a link to Ruby:
```
cd c:\dev\sonic-pi\app\server\native
mklink /d ruby c:\Ruby26-x64
```

4) Build the Application by running the prebuild first, then config.  These tools are in the app/gui/qt folder.  Note: If you have previously installed libaubio5-dll, or set the AUBIO_LIB environment variable, now is the time to remove the dll and remove the variable; the prebuild on windows will make and install the correct library for you.
``` 
cd app/gui/qt
win-prebuild.bat OR win-prebuild32.bat (to make the translations from ruby)
win-config.bat OR win-config32.bat (to make a project file)

cd build OR build32
EITHER:
cmake --build . --config Release
OR
Load the solution file from the build directory into Visual Studio and build it.
```

## Building the Server

Sonic Pi has most of its functionality inside the ruby interface, which also uses SuperCollider as its synthesizer.

1) Install Super Collider, 64 or 32 bit.  The current tested version is:
```
https://github.com/supercollider/supercollider/releases/download/Version-3.10.4/SuperCollider-3.10.4_Release-x64-VS-95e9507.exe
```

2) Copy these from C:\Program Files\SuperCollider-3.10.4 to C:\dev\sonic-pi\app\server\native
```
	lib*.dll
	msvcp140.dll
	scsynth.exe
	vcruntime140.dll
	plugins
```

Done!
Run C:\dev\sonic-pi\app\gui\qt\build\Release\sonic-pi.exe or build32\Release\sonic-pi.exe

## Tips
- Error logs are written to %USERPROFILE%/.sonic-pi/logs, and are useful to diagnose any startup problems.
- If a rebuild errors at the final stage of copying files, or you are otherwise having trouble starting sonic pi, there is win-killprocess.bat to remove sonic pi from memory.  This will also kill supercollider if it has been left running.- 32bit and 64bit don't mix.  Build the one you want in a clean tree.  Make sure you also install all the right 32/64 bit components to match your build.  64 bit is recommended on modern machines.
- `cd %QT_INSTALL_LOCATION%` will take you to the directory you have set for that environment variable - a good way to check you have set it up correctly
- You should find a libaubio-5.dll in your native/ruby/bin folder, if the prebuild has worked correctly.  This is required for the onset feature.


