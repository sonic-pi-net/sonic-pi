# SONIC PI DETAILED BUILD YOUR OWN INSTRUCTIONS FOR WINDOWS 10 BASED ON LATEST SP 2.11dev

Last revision 23rd December 2017  

Tested on Windows 10 64bit.

This document details the process from beginning to end to build Sonic Pi 2.11dev and I hope it will be useful to others, and encourage them to have a go themselves.

First there is quite a bit of software to install to facilitate the build, not least the source files for Sonic Pi.

First install **git** from https://git-scm.com/ click **download**, then run the installer file. I selected options **Use git from Windows Command Prompt**, **Checkout Windows-style**, **commit Unix-style line endings**, **Use Windows default console window**, and **Enable file system caching**

Now install the **Sonic Pi source files**. I put mine at the top level of the c: drive
Start a command window by typing **cmd** in the search field
```
cd c:/
git clone https://github.com/samaaron/sonic-pi.git
```

Now download **Visual Studio Express 2013 for Windows Desktop with Update 5** from
https://www.visualstudio.com/downloads/download-visual-studio-vs
scroll down the page and find **Visual Studio 2013**, and then **Express 2013 for Desktop**.
You can either choose **Web Installer** or **ISO** depending upon whether you want to install over the web or download a complete 3.3Gb and install from that. I did the full download, mounted the iso disk image and installed from that.

Download **Qt**. use **version 5.5.1** as version 5.4.x is NOT qualified for Windows 10
I went to http://www.qt.io/download-open-source/#section-2
And selected **Qt 5.5.1 for Windows 32-bit (VS 2013, 804 MB)** 

http://download.qt.io/official_releases/qt/5.5/5.5.1/qt-opensource-windows-x86-msvc2013-5.5.1.exe
After download, I ran the installer, accepting the default settings, which installs Qt at
```C:\Qt\Qt5.5.1``` (I skipped setting up an account!)

Install **cmake** from the link http://www.cmake.org/download
Scroll down and select ```cmake-3.4.1-win32-x86.exe``` and download.
Run the installer, selecting the option **Add to the System PATH for all (or current) users**


Install **Ruby** from http://rubyinstaller.org/downloads/
I choose **Ruby 2.3.0**.
Choose the **32 bit version**
Run the installer when downloaded. Choose the default install ```c:\Ruby23```
Select **add Ruby Executables to your path** and **associate .rb and .rbw files with this installation**.

Scroll further down the page of http://rubyinstaller.org/downloads/ and select
```DevKit-mingw64-32-4.7.2-20130224-1151-sfx.exe``` and download that as well
Make sure you use the **32 bit version for Ruby 2.0 and higher**.
When downloaded, run this and set the extract location to ```c:\RubyDev```

Now you have to complete the installation as follows:
In a command window
```
cd c:\RubyDev
ruby dk.rb init
```

This will create a file ```config.yml``` inside the ```RubyDev``` folder
Check its contents by typing;
```Notepad config.yml```
You should see a line ``` - C:\Ruby23```
at the end of the file.
Close notepad
Now type
```ruby dk.rb install```

Now you add two files from **pkgconfiglite**
Open the link
http://sourceforge.net/projects/pkgconfiglite/files
and select
**Looking for the latest version? Download pkg-config-lite-0.28-1_bin-win32.zip (47.7 kB)**
When the file has downloaded, extract and then copy the file ```pkg-config``` from the ```bin``` folder to ```c:\RubyDev\bin```
and the file ```pkg.m4``` from the ```share\aclocal``` folder to ```c:\RubyDev\share\aclocal```

That completes the setup for RubyDev

### Preparing the Ruby section
This section of the Sonic Pi install.md file should be completed next BEFORE attempting the section **Qt GUI** (unlike in the original install.md document) as the final build in this section requires one of the gems to be installed before it is run.

Copy ```c:\ruby23\*``` into ```c:\sonic-pi\app\server\native\windows\ruby```

You will need to create the last two folders windows\ruby before doing this, which is best done from a File Explorer window. The copying is also best done using **two** File Explorer windows, one set to ```c:\Ruby23``` and the other to ```c:\sonic-pi\app\server\native\windows\ruby```
You can leave out the **Doc** folder and the two **unins000** files.
Now open a cmd window and navigate to c:\sonic-pi\app\server\native\windows\ruby
From there we will install gem files required by Sonic Pi

```.\bin\gem install did_you_mean```

(allow access if you are asked). The install may take a little time. Eventually it will say two gems installed (did_you_mean and interception)

```.\bin\gem install ffi --platform=ruby```


Another gem recently added, that is needed specifically for the windows install is ```win32-process```

```.\bin\gem install win32-process```

The fourth gem file is problematical, and needs a patch to install. It goes like this:

```.\bin\gem fetch rugged```

This will fetch the latest version which is rugged-0.24.0

```
.\bin\gem unpack rugged-0.24.0.gem
.\bin\gem spec rugged-0.24.0.gem --ruby > rugged-0.24.0\rugged.gemspec
```

Now we have to apply a patch to one of the files.

```
cd rugged-0.24.0\ext\rugged
notepad extconf.rb
```
In the notepad window select **Find** from the Edit Menu (**ctrl+F**) and search for

```Unix Makefiles\””)```

and change it to read

```Unix Makefiles\" -DCMAKE_INSTALL_PREFIX=C:/ -DWINHTTP=OFF -DUSE_OPENSSL=OFF")```

Also, some lines below this, comment the 2 lines inside the ```if windows?``` putting a ```#``` in front of them, so that they read:
```
      if windows?
#        $LDFLAGS << " " + "-L#{Dir.pwd}/deps/winhttp"
#        $LIBS << " -lwinhttp -lcrypt32 -lrpcrt4 -lole32"
      else
```

NB: this may no longer be needed with more recent versions of rugged

Then resave the file and quit notepad.
Go back to the cmd window

```cd ..\..\```
 
you should be back in the folder ```c:\sonic-pi\app\server\native\windows\ruby\rugged-0.24.0```

Now build the patched rugged

```
..\bin\gem build rugged.gemspec
..\bin\gem install rugged-0.24.0.gem --platform=ruby
```

Now, install the fast_osc gem
```
..\bin\gem install fast_osc
```

NB: rb-native is not being used for Windows packages, the gems can happily live where they were installed by the "local" Ruby.  You will probably need to delete the vendor/rugged directory so it uses the right version.

That completes the ruby preparation work. Close the cmd and FileExplorer windows.


### Preparing the Qt GUI
From the **Start Button** select **All apps** and scroll down to **Visual Studio 2013**, and then to **Visual Studio Tools**. From the Window that opens **right click**  **VS2013 x86 Native Tools Command Prompt** and **Pin it to the Taskbar**. **Double click it** there to open a window which should be titled **VS2013 x86 Native Tools Command Prompt**.

Type **path** in that window after the prompt and press the enter key.

You will see a long string of folder locations which comprise the path associated with the window, which it will automatically search. It includes links to the various elements of visual studio. However, we have to add one more item, which is the path of the Qt bin directory.

Since the Visual Studio prompt path is rebuilt each time a window is opened, I have found the convenient way to do this is to create a **batch file** which can be run each time to add the additional entry. Since I want to store this at the C:\ top level, we have to run the cmd window with administrative privileges to create it.
Close the open Visual Studio Command Prompt window, then reopen it, holding down **Shift** and **Ctrl** when you do so. Say **Yes** to the popup window, and you will then have a cmd window with admin privileges. 

```
cd c:\
notepad
```

In the open notepad window type
```
PATH=C:\Qt\Qt5.5.1\5.5\msvc2013\bin;%PATH%
```

Then select **save** and save the file as ```pathupdate.bat```, at the top level of ```c:\``` Make sure you select **All Files** for the type so the extension can be set to **.bat** and NOT **.txt**

Close notepad and type ```dir``` (and press return) You should see the file ```pathupdate.bat``` listed.

Close the cmd window and double click the icon on the taskbar to reopen it with "normal" privileges.

```
cd c:\
pathupdate.bat
```

You should see the long path as before, but now with an additional entry ```C:\Qt\Qt5.5.1\5.5\msvc2013\bin``` at the end
Leave this window open.

Download **Qscintilla** from https://www.riverbankcomputing.com/software/qscintilla/download

Select the ```Qscintilla_gpl-2.9.2.zip``` file to download
When downloaded extract the ```Qscintilla_gpl-2.9.2``` folder to ```c:\```

Return to the visual studio command window. 
```
cd c:\QScintilla_gpl-2.9.2\Qt4Qt5
qmake qscintilla.pro CONFIG+=staticlib
nmake
nmake install
```
keep the cmd window open

copy ```Qt4Qt5\release\moc_qsciscintilla.cpp``` and ```moc_qsciscintillabase.cpp``` to 
```c:\sonic-pi\app\gui\qt\platform\win``` (There may be two files there already, but overwrite them)
Best done using two FileExplorer windows as before.

Next step is to install QWT.
Download the installer from https://sourceforge.net/projects/qwt/files/qwt/6.1.2/. Select the qwt-6.1.2.zip file.
Once downloaded, extract in c:\qwt-6.1.2.
In one **VS2013 x86 Native Tools Command Prompt**, cd into c:\qwt-6.1.2 and execute the following:
```
qmake qwt.pro
nmake
nmake install
```

And now, install Boost C++ libs:
Download Boost 1.61 from http://www.boost.org/users/history/version_1_61_0.html, and decompress into c:\.

And now we compile the binary boost files. Go to c:\boost_1_61_0, and execute the following:
```
bootstrap
b2 toolset=msvc-12.0
```



Create the C:\sonic-pi\app\server\rb-native\windows\2.3.0 directory.

Now we do the main build of Sonic-Pi gui, using the open cmd window.

```
cd c:\sonic-pi
.\app\gui\qt\win-build-app.bat
```
This should carry out the build. Don’t worry about various warnings en route.
We now add some files from visual studio which will be needed at run time.
Open two FileExplorer windows. In the first navigate to:

```Local Disk (C:)> Program Files (x86) > Microsoft Visual Studio 12.0 > VC > redist > x86 > Microsoft.VC120.CRT```

In the second to

```Local Disk (C:)> sonic-pi> app > gui > qt > release```

Copy msvcp120.dll and msvcr120.dll from the former to the latter.

In the second window navigate to **sonic-pi.exe** and **right-click** it to create a shortcut. Drag the shortcut to the desktop. Do NOT attempt to run it yet.

###  The final stage deals with SuperCollider
Install SuperCollider from http://supercollider.github.io/download.html
Select the **Win32 3.6.6** installer and download it

Run the installer

Download additional SuperCollider plugins from

http://sourceforge.net/projects/sc3-plugins/files/sc3-plugins%20Windows/

Select the latest version link at the top of the page and download
```Looking for the latest version? Download sc3-plugins-3.6.0-win32.zip (10.3 MB)```
Extract them in the download folder.

NB: The current Sonic Pi 2.11 is packaged with sc3-plugins 3.7.0-beta (f978dc2), which can be compiled in an MSYS environment as follows:
* install FFTW3 binaries from ftp://ftp.fftw.org/pub/fftw/fftw-3.3.4-dll32.zip to `/home/sc/fftw3.3.4-dll32` for example
* `git clone https://github.com/supercollider/sc3-plugins.git`
* `cd sc3-plugins && git submodule init && git submodule update`
* `mkdir build && cd build`
* `cmake -DSC_PATH=/home/sc/sources/supercollider/ -DFFTW3F_LIBRARY=/home/sc/fft w-3.3.4-dll32/libfftw3f-3.dll -DFFTW3F_INCLUDE_DIR=/home/sc/fftw3.3.4-dll32 -G "Unix Makefiles" ..`
* manually add `-IC:/msys/home/sc/fftw-3.3.4-dll32/` to `build/source/CMakeFiles/PitchDetection.dir/includes_CXX.rsp` (no idea why)
* make install
* copy SCX files from `c:\program files (x86)\sc3-plugins` or wherever it installs them

We now copy files to the sonic-pi installation.

With two FileExplorer windows navigate the first to ```Local Disk (C:) >Program Files (x86) > SuperCollider```

And the second (destination) to ```Local Disk (C:)> sonic-pi > app > server > native > windows```

Copy the ```plugins``` folder from the former to the latter.

Copy all dll files **EXCEPT those starting with QT** from the former to the latter.

Copy ```scsynth.exe``` from the former to the latter.

Keep these windows open for the moment.

Open the ```sc3-plugins-3.6.0-win32``` folder in the Downloads folder and navigate to ```SC3-plugins```.
Copy all the *.scx files that folder into the ```plugins``` folder copied to destination folder in the previous step.

Close the open FileExplorer windows.

There is one final patch to install. The Ugen file associated with the Piano synth was updated in the summer, to eliminate a problem with the tuning. Unfortunately the current Ugen has NOT yet been updated for windows, and consequently we have to use a modified version of the MdaUgen. This can be downloaded from https://goo.gl/K316fw
Extract the contents and follow the instructions in the readme file. Basically you replace the file ```sonic-pi-piano.scsyndef``` in ```c:\sonic-pi\etc\synthdefs\compiled``` with the one downloaded in the patch.

(You may like to retain a copy of the original before overwriting it, or perhaps rename it to ```sonic-pi-piano.scsyndef.original```)

### Finished!
That should complete the installation of Sonic Pi.
Try running it by double clicking the Sonic Pi shortcut on the desktop.


Robin Newman, 17th December 2015
