#!/usr/bin/env ruby

dir = File.dirname(File.expand_path(__FILE__))
$LOAD_PATH.unshift dir + "/../lib"

require "alsa-rawmidi"

# Selects the first MIDI input and prints the first 10 messages it receives to standard out

num_messages = 10

# AlsaRawMIDI::Device.all.to_s will list your midi devices
# or amidi -l from the command line

AlsaRawMIDI::Input.first.open do |input|

  puts "send some MIDI to your input now..."

  num_messages.times do
    m = input.gets
    puts(m)
  end

  puts "finished"

end
