dir = File.dirname(File.expand_path(__FILE__))
$LOAD_PATH.unshift dir + "/../lib"

require "test/unit"
require "mocha/test_unit"
require "shoulda-context"
require "coremidi"

module TestHelper
  
  def self.select_devices
    $test_device ||= {}
    { :input => CoreMIDI::Source.all, :output => CoreMIDI::Destination.all }.each do |type, devs|
      puts ""
      puts "select an #{type.to_s}..."
      while $test_device[type].nil?
        devs.each do |device|
          puts "#{device.id}: #{device.name}"
        end
        selection = $stdin.gets.chomp
        if selection != ""
          selection = selection.to_i
          $test_device[type] = devs.find { |d| d.id == selection }
          puts "selected #{selection} for #{type.to_s}" unless $test_device[type]
        end
      end
    end
  end 
	    
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

TestHelper.select_devices
