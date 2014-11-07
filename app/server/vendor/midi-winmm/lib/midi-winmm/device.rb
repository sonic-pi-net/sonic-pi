#!/usr/bin/env ruby
module MIDIWinMM
  
  #
  # Module containing methods used by both input and output devices when using the
  # WinMM driver interface 
  #
  module Device

    attr_reader :enabled, :id, :info, :name, :type
    
    alias_method :enabled?, :enabled
    
    def initialize(id, name, options = {}, &block)
      @id = id
      @info = options[:info]
      @type = self.class.name.split('::').last.downcase.to_sym
      @name = name
    end    
    
    def self.all
      all_by_type.values.flatten
    end
    
    def self.all_by_type
      types = { :input => Input,
              :output => Output }
      available_devices = { :input => [], :output => [] }
      count = 0
      types.each do |type, klass|
       (0..(Map::cfunc(type, :getNumDevs)-1)).each do |i|
          data = Map::DeviceInfo[type].new
          Map::cfunc(type, :getDevCapsA, i, data.to_ptr, Map::DeviceInfo[type].size)
          name = data[:szPname].to_s
          dev = klass.new(i, name, :info => data)
          available_devices[type] << dev
        end
      end
      available_devices
    end
    
    # select the first device of type <em>type</em>
    def self.first(type)
      all_by_type[type].first
    end
    
    # select the last device of type <em>type</em>
    def self.last(type)
      all_by_type[type].last
    end
    
    WinmmCallbackFlag = 0x30000 # we plan to use a callback to collect events
    
    private
    
    # High word 
    #   High-order byte: Not used.
    #   Low-order byte: Contains a second byte of MIDI data (when needed).
    # Low word  
    #   High-order byte:  Contains the first byte of MIDI data (when needed).
    #   Low-order byte: Contains the MIDI status.
    #
    def dwmsg_to_array_of_bytes(m)
      # there has to be a better way to do this
      s = []
      m = m.to_s(16)
      (1..m.length).step(2) { |i| s << m[(m.length-i)-1, 2].hex }
      s
    end
    
    def error?(num)
      Map::error?(num)
    end
    
    def error(num)
      Map::error(num)
    end
    
  end
  
end
