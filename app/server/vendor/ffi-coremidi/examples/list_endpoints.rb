#!/usr/bin/env ruby

dir = File.dirname(File.expand_path(__FILE__))
$LOAD_PATH.unshift dir + "/../lib"

require "coremidi"
require "pp"

# This will output a big list of Endpoint objects. Endpoint objects are what's used to input
# and output MIDI messages

pp CoreMIDI::Device.all.map { |device| device.endpoints.values }.flatten
