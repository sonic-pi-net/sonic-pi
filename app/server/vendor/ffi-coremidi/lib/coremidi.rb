# ffi-coremidi
#
# Realtime MIDI IO with Ruby for OSX
# (c)2011-2014 Ari Russo

# libs
require "ffi"
require "forwardable"

# modules
require "coremidi/api"
require "coremidi/endpoint"
require "coremidi/type_conversion"

# classes
require "coremidi/entity"
require "coremidi/device"
require "coremidi/source"
require "coremidi/destination"

module CoreMIDI
  VERSION = "0.3.5"
end
