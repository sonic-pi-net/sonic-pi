#!/usr/bin/env ruby

dir = File.dirname(File.expand_path(__FILE__))
$LOAD_PATH.unshift dir + '/../lib'

require 'midi-winmm'
require 'pp'

pp MIDIWinMM::Device.all_by_type

