#!/usr/bin/env ruby
module MIDIWinMM
  
  #
  # Output device class for the WinMM driver interface 
  #
  class Output
    
    include Device
    
    BufferSize = 2048
    
    attr_reader :buffer
   
    # initialize this device
    def enable(options = {}, &block)
      init_output_buffer
      Map.winmm_func(:midiOutOpen, Output::HandlePointer, @id, Output::EventCallback, 0, Device::WinmmCallbackFlag)
      @handle = HandlePointer.read_int
      @enabled = true
      unless block.nil?
        begin
          yield(self)
        ensure
          close
        end
      else
        self
      end
    end
    alias_method :start, :enable
    alias_method :open, :enable

    # close this device
    def close
      Map.winmm_func(:midiOutClose, @handle)
      @enabled = false
    end
    
    # returns a hash of fixnum values { :left => n, :right => n2 } 
    def volume
      volume = FFI::MemoryPointer.new(FFI.type_size(:ulong))
      
      Map.winmm_func(:midiOutGetVolume, @handle, volume)
      
      str = dwmsg_to_array_of_bytes(volume.read_ulong)
      left = str.slice!(0,4)
      
      { :left => left.hex, :right => str.hex }
    end
    
    # accepts either a hash of fixnums { :left => n, :right => n2 } or 
    # a single fixnum that will be applied to both channels
    def volume=(val)
      vol = val.kind_of?(Hash) ? (val[:left] + (val[:right] << 16)) : (val + (val << 16))
      Map.winmm_func(:midiOutSetVolume, @handle, vol)        
    end
    
    def reset
      Map.winmm_func(:midiOutReset, @handle)
    end
    
    # send a message of an indeterminate type
    def puts(*a)
      case a.first
        when Array    then puts_bytes(*a.first)
        when Numeric  then puts_bytes(*a) 
        when String   then puts_s(*a)
      end
    end
    
    # send a message consisting of Numeric bytes
    def puts_bytes(*message_bytes)
      format = "C" * message_bytes.size
      
      packed = message_bytes.pack(format)
      data_pointer = FFI::MemoryPointer.new(message_bytes.size).put_bytes(0, packed)
      
      @header[:dwBufferLength] = message_bytes.size
      @header[:dwBytesRecorded] = message_bytes.size
      @header[:lpData] = data_pointer
      
      Map.winmm_func(:midiOutPrepareHeader, @handle, @header.pointer, @header.size)
      
      Map.winmm_func(:midiOutLongMsg, @handle, @header.pointer, @header.size)
      
    end
    
    # send a message consisisting of a String of hex digits 
    def puts_s(data)
      data = data.dup
	  output = []
      until (str = data.slice!(0,2)).eql?("")
      	output << str.hex
      end
      puts_bytes(*output)
    end
    alias_method :puts_bytestr, :puts_s
    alias_method :puts_hex, :puts_s
    
    def self.first
      Device::first(:output)
    end

    def self.last
      Device::last(:output)
    end
    
    def self.all
      Device.all_by_type[:output]
    end
    
    private
    
    EventCallback = Proc.new do |hmo, wMsg, dwInstance, dwParam1, dwParam2|
      msg_type = Map::CallbackMessageTypes[wMsg] || ''
      if msg_type.eql?(:output_data)
        header = dwParam1
        handle = HandlePointer.read_int
        Map.winmm_func(:midiOutUnprepareHeader, handle, header, Map::MIDIHdr.size)
      end
    end
    
    HandlePointer = FFI::MemoryPointer.new(FFI.type_size(:int))
    
    def init_output_buffer
      @header = Map::MIDIHdr.new
      d = FFI::MemoryPointer.new(:char, Output::BufferSize)
      d.put_string(0, " " * (Output::BufferSize - 1)) # initialize with a string of spaces
      @header[:lpData] = d.address
      @header[:dwBufferLength] = Output::BufferSize
      @header[:dwBytesRecorded] = 0
      @header[:dwFlags] = 0
      @header[:dwUser] = 0
    end
       
  end
  
end
