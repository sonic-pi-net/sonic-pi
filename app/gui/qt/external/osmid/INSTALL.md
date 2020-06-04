# Build instructions
osmid uses cmake for build management.

## Requisites:
### Windows:
* cmake (version 3.0 or later)
* Microsoft Visual C++ 2015 or Visual C++ 2017 or Visual C++ 2019 (Community or Express editions are fine)

### Linux (you can normally install all these using your distribution package manager)
* cmake (version 3.0 or later)
* gcc compiler (4.9 or later)
* ALSA development libraries

### Mac
Don't know yet...


## Building
* go to the root directory where you have the osmid sources (probably where this file is)
* create `build` directory, and get into it (`mkdir build`, and then `cd build`)
* It is normally enough to do `cmake ..`. If cmake complains about not finding a library make sure that it is installed.
* On Linux, if cmake was succesful then it created the necessary Makefiles. Do `make`, and that will compile the code and create the binaries.
* On Windows, if cmake was successful then it created the necessary Visual Studio solution and Project files. Open the `osmid.sln` file, and Build normally to create the binaries.
  Example of CMake commands.
  ```
  mkdir build
  cd build
  cmake -G "Visual Studio 16 2019" -A x64 ..
  ```



