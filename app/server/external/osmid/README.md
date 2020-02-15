[![Build Status](https://travis-ci.org/llloret/osmid.svg?branch=master)](https://travis-ci.org/llloret/osmid)

# osmid

osmid aims to provide a lightweight, portable, easy to use tool to convert MIDI to OSC and OSC to MIDI.

I have started it as a proof of concept for the MIDI components for Sonic PI. I started playing a bit with MidiOSC, but after a while I decided to implement my own tools for more flexibility on the design and architecture.

The idea now if to see what features are required (or not), and use that to evolve the project. Of course, osmid is available for other projects and uses too, so any ideas and feedback are very welcome.

osmid is divided in 2 tools:
* m2o: MIDI to OSC conversion
* o2m: OSC to MIDI conversion

Having two separate tools follows Unix ideas of having a number of smaller standalone tools instead of bigger monolithic ones. Since some projects might want to use just one direction for the conversion, it makes sense to keep this separation.

## m2o features
* Portable: Works under Windows, Linux and Mac
* Compact
* Very low latency
* Customizable: can open a number of midi inputs, or all, and can configure the output UDP socket (if it is necessary to forward differnet MIDI devices to different OSC destinations, it is possible to do that invoking the program multiple times, with the appropriate arguments)
* OSC address templates, that is, the format of the OSC address can be passed as an argument to the program. If the template parameter is not passed, then templates are NOT used (for example, if you REALLY care about latency - but we are talking about tens of microseconds here...).


## o2m features
* Portable: Works under Windows, Linux and Mac
* Compact
* Very low latency
* For a list of the OSC messages that o2m supports see below in "o2m incoming OSC message format"

## Building
For build instruction see INSTALL.md.

osmid is built assuming C++14. The build system is based on cmake. Tested target compiler in Windows is MSVC 2015 Win64, in Linux is gcc 4.9 or later, and on Mac, clang 5.1
or later. Under Windows, prepare using something like: `cmake -G "Visual Studio 14 2015 Win64" .. `. On Linux and Mac `cmake ..` should be enough.

osmid uses the following libs:
* JUCE for the midi handling and OSC handling (included in the tree)
* oscpack, for the OSC handling and UDP networking (included in the tree)
* spdlog, for logging (included in the tree)
* cxxopts, for program options parsing (included in the tree)



## m2o parameters
* --midiin or -i <MIDI Input device>: open the specified input device - can be specified multiple times to open more than one device. By default it will open all input devices and the ones that are connected live
* --oschost or -H <hostname or IP address>: send the OSC output to the specified host
* --oscport or -o <UDP port number>: send the OSC output to the specified port - can be specified multiple times to send to more than one port
* --osctemplate or -t <OSC template>: use the specified OSC output template (use $n: midi port name, $i midi port id, $c: midi channel, $m: message_type). For example: -t /midi/$c/$m
* --oscrawmidimessage or -r: send the raw MIDI data in the OSC message, instead of a decoded version
* --monitor or -m: logging level. Number from 0 to 6. Smaller numbers are more verbose
* --list or -l: List input MIDI devices
* --heartbeat or -b: sends OSC heartbeat message
* --help: Display this help message
* --version: Show the version number

## m2o outgoing OSC message format
The address by default is: /midi/<port id>/<channel>(if the message contains channel information).
The address can be templated with the -t argument.

The message body is:
* By default: (int)<port id>, (string)<port name>, <decoded message data>(i.e. for note_on messages, it will be 2 integers: note, velocity)
* if -r specified: (int)<port id>, (string)<port name>, (blob)<raw midi data>.


There is also an optional heartbeat message which sends periodic messages with the following format:
OSC address pattern: /midi/heartbeat. Message body is OSC array of pairs <midi device id>, <midi device name>


## o2m parameters (Need to update this)
* --list or -l: List output MIDI devices
* --midiout or -o: open the specified output device - can be specified multiple times to open more than one device. By default it will open all output devices and the ones that are connected live
* --oscport or -i: OSC Input port (default:57200)
* --heartbeat or -b: sends OSC heartbeat message. See oscoutputhost and oscoutputport arguments.
* --oscoutputhost or -H, host to send OSC messages to (default:127.0.0.1). Used for heartbeat
* --oscoutputport or -O:host to send OSC messages to (default:57120). Used for heartbeat
* --monitor or -m: logging level. Number from 0 to 6. Smaller numbers are more verbose
* --help: Display this help message
* --version: Show the version number


## o2m incoming OSC message format
- The expected OSC address pattern is /(string)"out midi device name or global"/(string)"midi command".
  You can use * in the device name to send to all devices
- Recognized midi commands, and the expected OSC body:
	- raw: send a midi command as is. Body can be either a blob or a sequence of int32s
	- note_on: Body is (int32)channel, (int32)note, (int32)velocity
	- note_off: Body is (int32)channel, (int32)note, (int32)velocity
	- control_change: Body is (int32)channel, (int32)control number, (int32)control value
	- pitch_bend: Body is (int32)channel, (int32)value
	- channel_pressure: Body is (int32)channel, (int32)value
	- poly_pressure: Body is (int32)channel, (int32)note, (int32)value
	- program_change: Body is (int32)channel, (int32)program number
 	- clock: Body is empty
	- start: Body is empty
	- stop: Body is empty
	- continue: Body is empty
	- active_sense: Body is empty
	- log_level: Body is (int32)log_level. Value from 0 to 6. The smaller the number the more verbose the output.
	- log_to_osc: Body is (int32)enable. 0 -> disable, 1 -> enable

## TODO
* Installers

## LICENSE
See LICENSE.md file for details.
