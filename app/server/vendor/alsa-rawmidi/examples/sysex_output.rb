#!/usr/bin/env ruby

dir = File.dirname(File.expand_path(__FILE__))
$LOAD_PATH.unshift dir + "/../lib"

# Sends a MIDI system exclusive message to the selected output

require "alsa-rawmidi"

output = AlsaRawMIDI::Output.first
sysex_msg = [0xF0, 0x41, 0x10, 0x42, 0x12, 0x40, 0x00, 0x7F, 0x00, 0x41, 0xF7]

output.open { |output| output.puts(sysex_msg) }
