dir = File.dirname(File.expand_path(__FILE__))
$LOAD_PATH.unshift dir + '/../lib'

require 'test/unit'
require 'midi-winmm'

module TestHelper
  
  def bytestrs_to_ints(arr)
    data = arr.map { |m| m[:data] }.join
    output = []
    until (bytestr = data.slice!(0,2)).eql?("")
      output << bytestr.hex
    end
    output      
  end
  
  # some MIDI messages
  VariousMIDIMessages = [
    [0xF0, 0x41, 0x10, 0x42, 0x12, 0x40, 0x00, 0x7F, 0x00, 0x41, 0xF7], # SysEx
    [0x90, 100, 100], # note on
    [0x90, 43, 100], # note on
    [0x90, 76, 100], # note on
    [0x90, 60, 100], # note on
    [0x80, 100, 100] # note off
  ]
  
  # some MIDI messages
  VariousMIDIByteStrMessages = [
    "F04110421240007F0041F7", # SysEx
    "906440", # note on
    "804340" # note off
  ]
  
end

require File.dirname(__FILE__) + '/config'