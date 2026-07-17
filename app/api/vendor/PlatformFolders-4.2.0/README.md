# PlatformFolders [![Build Status](https://travis-ci.org/sago007/PlatformFolders.svg?branch=master)](https://travis-ci.org/sago007/PlatformFolders) [![AppVeyor](https://img.shields.io/appveyor/ci/sago007/PlatformFolders.svg?label=Windows)](https://ci.appveyor.com/project/sago007/platformfolders) [![license](https://img.shields.io/github/license/sago007/PlatformFolders.svg)](https://raw.githubusercontent.com/sago007/PlatformFolders/master/LICENSE) [![Join the chat at https://gitter.im/PlatformFolders/Lobby](https://badges.gitter.im/PlatformFolders/Lobby.svg)](https://gitter.im/PlatformFolders/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Codacy Badge](https://api.codacy.com/project/badge/Grade/729e36adcf5c4523bd136de1b33441cb)](https://www.codacy.com/app/github_43/PlatformFolders?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=sago007/PlatformFolders&amp;utm_campaign=Badge_Grade)

A C++ library to look for directories like `My Documents`, `~/.config`, `%APPDATA%`, etc. so that you do not need to write platform-specific code

[Source code](https://github.com/sago007/PlatformFolders) • [Latest release](https://github.com/sago007/PlatformFolders/releases/latest) • [Doxygen documentation](https://sago007.github.io/PlatformFolders/html/doxygen/)

## Rationale

There are a lot of platform abstraction libraries available. You can get graphics abstraction libraries, GUI abstraction libraries and file abstraction libraries.

But folder abstraction seems to be more difficult. My problem was that the code that found the place to save data was platform dependent. This cluttered my code and often I would not discover that it did not compile until moving it to the different platforms.

[I have written a bit more about it here.](https://sago007.blogspot.dk/2015/10/abstraction-for-special-folders.html)

There are some alternatives that you might consider instead:

* [QStandardPaths](https://doc.qt.io/qt-5/qstandardpaths.html)
* [glib](https://developer.gnome.org/glib/stable/glib-Miscellaneous-Utility-Functions.html)

Both are properly more mature than this library. However they are both parts of large frameworks and using them with libraries outside the framework may not be that simple.

## Operating System Support

### Windows

For Windows, the current version fetches the folders using SHGetKnownFolderPath. This requires Vista or newer.

### Linux

This library uses the [XDG user-dirs.](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)

It should work on any Unix system that has the following headers available: `pwd.h`, `unistd.h`, and `sys/types.h`

### macOS

Version 4.0.0 and forward uses hardcoded values for the directories on Mac OS X. Unlike the other operating systems the folders cannot be moved on a Mac and the translation is done in the UI. The versions 2.X and 3.X uses the deprecated FSFindFolder, which requires the CoreServices framework during linking. Version 1.X simple used the XDG specification.

## Usage

This project should be compatible with things like [Cmake's ExternalProject_Add](https://cmake.org/cmake/help/latest/module/ExternalProject.html?highlight=externalproject_add#command:externalproject_add) if you wish to use it in your project.

You can also follow the [build step](#building) below to install at a system level, and use [Cmake's find_package](https://cmake.org/cmake/help/latest/command/find_package.html).

```cmake
# Specifying a version is optional -- note it follows Semver
find_package(platform_folders 3.1.0 REQUIRED)
# Which imports the linkable library "sago::platform_folders"
# Use it like so...
target_link_libraries(EXEORLIBNAME PRIVATE sago::platform_folders)
```

Alternatively, you can just copy the [sago](https://github.com/sago007/PlatformFolders/tree/master/sago) folder into your program and manually link everything. If you use the last option and are using a library version from before 4.0.0: Remember to link to the CoreServices lib when compiling on Mac. This typically means passing "-framework CoreServices" during the linking phase.

Note that if you build in-tree, you can link against the Cmake alias `sago::platform_folders` just like if you had used find_package.

### Building

**Notes:**

* Until 4.0.0 macOS required the CoreServices framework during linking.
* If you don't want to install, remove the `--target install` command.

Linux/macOS:

```
mkdir -p build && cd build
cmake -DBUILD_TESTING=OFF -DCMAKE_BUILD_TYPE=Release ..
sudo cmake --build . --target install
```

Windows:

```
mkdir build && cd build
cmake -DBUILD_TESTING=OFF ..
runas /user:Administrator "cmake --build . --config Release --target install"
```

## Example Usage

This sample program gets all folders from the system:

```cpp
#include <sago/platform_folders.h>
#include <iostream>
#include <string>

int main()
{
	std::cout << "Config: " << sago::getConfigHome() << "\n";
	std::cout << "Data: " << sago::getDataHome() << "\n";
	std::cout << "State: " << sago::getStateDir() << "\n";
	std::cout << "Cache: " << sago::getCacheDir() << "\n";
	std::cout << "Documents: " << sago::getDocumentsFolder() << "\n";
	std::cout << "Desktop: " << sago::getDesktopFolder() << "\n";
	std::cout << "Pictures: " << sago::getPicturesFolder() << "\n";
	std::cout << "Music: " << sago::getMusicFolder() << "\n";
	std::cout << "Video: " << sago::getVideoFolder() << "\n";
	std::cout << "Download: " << sago::getDownloadFolder() << "\n";
	std::cout << "Save Games 1: " << sago::getSaveGamesFolder1() << "\n";
	std::cout << "Save Games 2: " << sago::getSaveGamesFolder2() << "\n";
	return 0;
}
```

### Example Output

#### On Linux

```
Config: /home/poul/.config
Data: /home/poul/.local/share
State: /home/poul/.local/state
Cache: /home/poul/.cache
Documents: /home/poul/Dokumenter
Desktop: /home/poul/Skrivebord
Pictures: /home/poul/Billeder
Music: /home/poul/Musik
Video: /home/poul/Videoklip
Download: /home/poul/Hentede filer
Save Games 1: /home/poul/.local/share
Save Games 2: /home/poul/.local/share
```

#### On Windows

```
Config: C:\users\poul\Application Data
Data: C:\users\poul\Application Data
State: C:\users\poul\Local Settings\Application Data
Cache: C:\users\poul\Local Settings\Application Data
Documents: C:\users\poul\Mine dokumenter
Desktop: C:\users\poul\Skrivebord
Pictures: C:\users\poul\Mine Billeder
Music: C:\users\poul\Min Musik
Video: C:\users\poul\Mine Film
Download: C:\users\poul\Downloads
Save Games 1: C:\users\poul\Mine dokumenter\My Games
Save Games 2: C:\users\poul\Saved Games
```

#### On macOS

```
Config: /Users/poul/Library/Application Support
Data: /Users/poul/Library/Application Support
State: /Users/poul/Library/Application Support
Cache: /Users/poul/Library/Caches
Documents: /Users/poul/Documents
Desktop: /Users/poul/Desktop
Pictures: /Users/poul/Pictures
Music: /Users/poul/Music
Video: /Users/poul/Movies
Download: /Users/poul/Downloads
Save Games 1: /Users/poul/Library/Application Support
Save Games 2: /Users/poul/Library/Application Support
```

## Compiler Compatibility

Versions up to 3.X.X should compile with any C++98 compiler.\
Versions from 4.0.0 and up require a C++11 compatible compiler.

The aim is to always support the default C++ compiler on the oldest supported version of Ubuntu. This is a very basic library and it is not supposed to force you to upgrade.

## Encoding

From version 3.0, Windows always encodes to UTF-8, and this will be the default on almost any other system. Before version 3.0, Windows was encoded in ANSI. Although the user may use any characters they want, I recommend that the program should have only ASCII characters in the source code itself.

# Licence

Provided under the MIT license for the same reason XDG is licensed under it. So that you can quickly copy-paste the methods you need or just include the "sago"-folder.
