oscpack -- Open Sound Control packet manipulation library
A simple C++ library for packing and unpacking OSC packets.
http://www.rossbencina.com/code/oscpack

Copyright (c) 2004-2013 Ross Bencina <rossb@audiomulch.com>


Oscpack is simply a set of C++ classes for packing and unpacking OSC packets. 
Oscpack includes a minimal set of UDP networking classes for Windows and POSIX.
The networking classes are sufficient for writing many OSC applications and servers, 
but you are encouraged to use another networking framework if it better suits your needs. 
Oscpack is not an OSC application framework. It doesn't include infrastructure for 
constructing or routing OSC namespaces, just classes for easily constructing, 
sending, receiving and parsing OSC packets. The library should also be easy to use 
for other transport methods (e.g. serial).

The key goals of the oscpack library are:

    - Be a simple and complete implementation of OSC
    - Be portable to a wide variety of platforms
    - Allow easy development of robust OSC applications 
      (for example it should be impossible to crash a server 
      by sending it malformed packets, and difficult to create 
      malformed packets.)

Here's a quick run down of the key files:

osc/OscReceivedElements -- classes for parsing a packet
osc/OscPrintRecievedElements -- iostream << operators for printing packet elements
osc/OscOutboundPacketStream -- a class for packing messages into a packet
osc/OscPacketListener -- base class for listening to OSC packets on a UdpSocket
ip/IpEndpointName -- class that represents an IP address and port number
ip/UdpSocket -- classes for UDP transmission and listening sockets
tests/OscUnitTests -- unit test program for the OSC modules
tests/OscSendTests -- examples of how to send messages
tests/OscReceiveTest -- example of how to receive the messages sent by OSCSendTests
examples/OscDump -- a program that prints received OSC packets
examples/SimpleSend -- a minimal program to send an OSC message
examples/SimpleReceive -- a minimal program to receive an OSC message

osc/ contains all of the OSC related classes
ip/ contains the networking classes

ip/windows contains the Windows implementation of the networking classes
ip/posix contains the POSIX implementation of the networking classes


Building
--------

The idea is that you will embed this source code in your projects as you 
see fit. The Makefile has an install rule for building a shared library and 
installing headers in usr/local. It can also build a static library.
There is a CMakeLists.txt for building with cmake.

Makefile builds
...............

The Makefile works for Linux and Max OS X. It should also work on other platforms
that have make. Just run:

$ make

You can run "make install" if you like.


Cmake builds
............

There is a CMakeLists.txt file which has been tested with cmake on 
Windows and Linux. It should work on other platforms too.
For example, to generate a Visual Studio 10 project, run cmake 
like this:

> cmake -G "Visual Studio 10"

Run cmake without any parameters to get a list of available generators.


Mingw build batch file
......................

For Windows there is a batch file for doing a simple test build with 
MinGW gcc called make.MinGW32.bat. This will build the test executables 
and oscdump in ./bin and run the unit tests.


Note:

In some rare instances you may need to edit the Makefile or 
osc/OscHostEndianness.h to configure oscpack for the endianness of your 
processor (see the comments at the top of the Makefile for details).



Verification test
-----------------

To run the unit tests:

$ ./bin/OscUnitTests

To run the send and receive tests. Open two terminals. In one run:

$ ./bin/OscReceiveTest

Then in the other terminal run:

$./bin/OscSendTests


You should see an indication that the messages were received 
in the first terminal.

Note that OscSendTests intentionally sends some unexpected
message parameters to test exception handling in the receiver.
You will see some "error while parsing message" messages printed. 

You can use ./bin/OscDump to print out OSC messages received
from any program, including the test programs.


--


If you fix anything or write a set of TCP send/receive classes 
please consider sending me a patch. My email address is 
rossb@audiomulch.com. Thanks :)

For more information about Open Sound Control, see:
http://opensoundcontrol.org/

Thanks to Till Bovermann for helping with POSIX networking code and
Mac compatibility, and to Martin Kaltenbrunner and the rest of the
reacTable team for giving me a reason to finish this library. Thanks
to Merlijn Blaauw for reviewing the interfaces. Thanks to Xavier Oliver
for additional help with Linux builds and POSIX implementation details.

Portions developed at the Music Technology Group, Audiovisual Institute, 
University Pompeu Fabra, Barcelona, during my stay as a visiting
researcher, November 2004 - September 2005.

Thanks to Syneme at the University of Calgary for providing financial 
support for the 1.1.0 update, December 2012 - March 2013.

See the file CHANGES for information about recent updates.

See the file LICENSE for information about distributing and using this code.

###
