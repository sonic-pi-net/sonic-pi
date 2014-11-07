dir = File.dirname(File.expand_path(__FILE__))
$LOAD_PATH.unshift dir + '/../lib'

require "test/unit"
require "mocha/test_unit"
require "shoulda-context"
require "unimidi"

module TestHelper

  extend self

  def sysex_ok?
    ENV["_system_name"] != "OSX" || !RUBY_PLATFORM.include?("java")
  end

  def devices
    if @devices.nil?
      @devices = {}
      { :input => UniMIDI::Input, :output => UniMIDI::Output }.each do |type, klass|
        @devices[type] = klass.gets
      end
    end
    @devices
  end

  def bytestrs_to_ints(arr)
    data = arr.map { |m| m[:data] }.join
    output = []
    until (bytestr = data.slice!(0,2)).eql?("")
      output << bytestr.hex
    end
    output    	
  end

  class MIDIObj
    def initialize(*bytes)
      @bytes = bytes
    end
    def to_bytes
      @bytes
    end
  end 

  def message_objects
    numeric_messages.map { |message| MIDIObj.new(*message) }
  end

  def numeric_messages
    messages = [
      [0x90, 100, 100], # note on
      [0x90, 43, 100], # note on
      [0x90, 76, 100], # note on
      [0x90, 60, 100], # note on
      [0x80, 100, 100] # note off
    ]
    if sysex_ok?
      messages << [0xF0, 0x41, 0x10, 0x42, 0x12, 0x40, 0x00, 0x7F, 0x00, 0x41, 0xF7]
    end
    messages
  end

  def string_messages
    messages = [
      "906440", # note on
      "804340" # note off
    ]
    if sysex_ok?
      messages << "F04110421240007F0041F7"
    end
    messages
  end

end
