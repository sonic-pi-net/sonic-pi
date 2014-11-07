#!/usr/bin/env ruby
#
# Set of modules and classes for interacting with MIDI functions 
# of the WinMM System API
#
module MIDIWinMM
  VERSION = "0.1.10"
end

require 'ffi'
 
require 'midi-winmm/device'
require 'midi-winmm/input'
require 'midi-winmm/map'
require 'midi-winmm/output'
