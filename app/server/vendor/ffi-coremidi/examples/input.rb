#!/usr/bin/env ruby

dir = File.dirname(File.expand_path(__FILE__))
$LOAD_PATH.unshift dir + "/../lib"

require "coremidi"

# This program selects the first midi input and sends an inspection of the first 10 messages
# messages it receives to standard out

num_messages = 10

# CoreMIDI::Device.all.to_s will list your midi devices
# or amidi -l from the command line

CoreMIDI::Source.all[0].open do |input|

  puts "Using input: #{input.id}, #{input.name}"

  puts "send some MIDI to your input now..."

  num_messages.times do
    m = input.gets
    puts(m)
  end

  puts "finished"

end
