###############################################
### Building the Latest Sonic Pi on Windows ###
###############################################

These instructions are current as of 25/1/2020, and detail how to build a 64 bit release of Sonic Pi on Windows.  Please let us know if you have any issues following these instructions.  The current build on windows is assumed to be a 64 bit build done with Visual Studio 2019 (Community or Pro).

Start by cloning the Sonic Pi Repository in the usual way
```
git clone https://github.com/samaaron/sonic-pi.git c:/dev/sonic-pi
``` 

# Build the UI

1) Install Qt.  Qt is not something you want to try building on windows; it is much better to install - be sure to pick the 5.12.6 checkbox to get the installed libraries; it is not on by default.  The following release is recommended/tested.  After install setup an environment variable to point to the install location.  I like to use Rapid Environment Editor to setup these variables permanently (https://www.rapidee.com/en/about).  Otherwise the setx command can make global variables, but the command line needs to be restarted afterwards.  The current recommended version is 5.12.6, but other versions should work.
```
http://download.qt.io/official_releases/qt/5.12/5.12.6/qt-opensource-windows-x86-5.12.6.exe
setx QT_INSTALL_LOCATION c:/Qt5.12.6  (restart command prompt)
```

2) Install the latest CMake http://www.cmake.org/download.  This is a build tool that is required

3) Some ruby is needed to run the translations and generate the help headers before the main build; so install the gems and do the one-time patch to aubio, which is required on windows.
Install Ruby from http://rubyinstaller.org/downloads/ Get the version with the devkit, 64 bit:
```
https://github.com/oneclick/rubyinstaller2/releases/download/RubyInstaller-2.6.5-1/rubyinstaller-devkit-2.6.5-1-x64.exe
gem install win32-process
gem install rugged --version 0.27.1
gem install aubio
```

In C:\Ruby25-x64\lib\ruby\gems\2.5.0\gems\aubio-0.3.1\lib\aubio\aubio-ffi.rb
Replace the ffi_lib "...dylib" line  with ffi_lib "#{File.dirname(__FILE__)}/libaubio-5.dll"
Download aubio from https://aubio.org/download (choose the aubio-0.x.x-win64.zip), copy the DLLs into the same aubio\ directory alongside aubio-ffi.rb

4) Build the Application by running the prebuild first, then config.
``` 
prebuild.bat (to make the translations from ruby)
config.bat (to make a project file)
cd build
EITHER:
cmake --build . --config Release
OR
Load the solution file from the build directory into Visual Studio and build it.
```

# Building the Server

Sonic Pi has most of its functionality inside the ruby interface, which also uses SuperCollider as its synthesizer.

## SuperCollider
1) Install Super Collider, 64 bit.  The current tested version is:
```
https://github.com/supercollider/supercollider/releases/download/Version-3.10.4/SuperCollider-3.10.4_Release-x64-VS-95e9507.exe
```

2) Copy these from C:\Program Files\SuperCollider-3.10.4 to C:\dev\sonic-pi\app\server\native
	lib*.dll
	msvcp140.dll
	scsynth.exe
	vcruntime140.dll
	plugins

## Ruby Components
Run a console as administrator.

1) Add a link to Ruby so the server can find it: In c:\dev\sonic-pi\app\server\native run:
```
mklink /d ruby c:\Ruby25-x64
```

2) Edit sonic-pi\app\server\ruby\lib\sonicpi\studio.rb 
```
 remove the three occurrences of the string "exec" leaving the rest of the three lines unchanged
 comment out the body of def __erl_mut_start_erlang
```

3) Sonic-pi/app/server/ruby/core.rb - comment out the line
```
require 'win32/process' if os == :windows
```

4) Comment these lines out from C:\dev\sonic-pi\app\server\ruby\bin\sonic-pi-server.rb
```
STDOUT.puts "Erlang port: #{erlang_port}"
ensure_port_or_quit.call(erlang_port, gui)
erlang_port = ARGV[6] ? ARGV[6].to_i : 4561
erlang_port: erlang_port,
```

5) Add the ".exe" to these lines in C:\dev\sonic-pi\app\server\ruby\lib\sonicpi\util.rb
```
File.join(native_path, "osmid", "o2m.exe")
File.join(native_path, "osmid", "m2o.exe")
```

6) Comment out the whole of C:\dev\sonic-pi\app\server\ruby\lib\sonicpi\gitsave.rb

Run C:\dev\sonic-pi\app\gui\qt\Release\sonic-pi.exe.

