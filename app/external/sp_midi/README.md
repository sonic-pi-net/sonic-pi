# sp_midi

sp_midi aims to provide a lightweight, portable, easy to use library for Sonic Pi to be able to work with MIDI devices.

It provides an erlang NIF interface to be integrated into Sonic Pi's erlang event dispatching architecture.

For the erlang side to drive the API it provides some NIF-based functions. On the other direction for the library to provide events to erlang it uses enif_send (also part of erlang's NIF).

## Building
For build instruction see INSTALL.md.

sp_midi is built assuming C++14. The build system is based on cmake. Tested target compiler in Windows is MSVC 2019 Win64, in Linux is gcc 4.9 or later, and on Mac, clang 5.1
or later.

sp_midi uses the following Open Source libs:
* spdlog, for logging (included in the tree)
* concurrentqueue (included in the tree)
* rtmidi (included in the tree)

## LICENSE
See LICENSE.md file for details.
