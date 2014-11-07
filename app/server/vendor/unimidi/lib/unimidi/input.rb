module UniMIDI

  # A MIDI input device
  class Input

    extend Device::ClassMethods
    include Device::InstanceMethods

    # All MIDI input devices -- used to populate the class
    # @return [Array<Input>]
    def self.all
      Loader.devices(:direction => :input)
    end

    # The device buffer
    # @return [Array<Hash>]
    def buffer
      @device.buffer
    end

    #
    # Plucks data from the input buffer and returns it as array of MIDI event hashes as such:
    #   [
    #     { :data => [144, 60, 100], :timestamp => 1024 },
    #     { :data => [128, 60, 100], :timestamp => 1100 },
    #     { :data => [144, 40, 120], :timestamp => 1200 }
    #   ]
    #
    # In this case, the data is an array of Numeric bytes
    # The timestamp is the number of millis since this input was enabled
    # Arguments are passed to the underlying device object
    #
    # @param [*Object] args
    # @return [Array<Hash>]
    def gets(*args)
      @device.gets(*args)
    rescue SystemExit, Interrupt
      exit
    end

    #
    # Plucks data from the input buffer and returns it as array of MIDI event hashes.
    # Similar to Input#gets except that the returned message data as string of hex digits eg:
    #   [
    #     { :data => "904060", :timestamp => 904 },
    #     { :data => "804060", :timestamp => 1150 },
    #     { :data => "90447F", :timestamp => 1300 }
    #   ]
    #
    # @param [*Object] args
    # @return [Array<Hash>]
    def gets_s(*args)
      @device.gets_s(*args)
    rescue SystemExit, Interrupt
      exit
    end
    alias_method :gets_bytestr, :gets_s
    alias_method :gets_hex, :gets_s

    #
    # Plucks data from the input buffer and returns it as an array of data bytes such as
    #   [144, 60, 100, 128, 60, 100, 144, 40, 120]
    #
    # @param [*Object] args
    # @return [Array<Fixnum>]
    def gets_data(*args)
      arr = gets(*args)
      arr.map { |msg| msg[:data] }.inject(:+)
    end

    #
    # Plucks data from the input buffer and returns it as a string of data such as
    #   "90406080406090447F"
    #
    # @param [*Object] args
    # @return [String]
    def gets_data_s(*args)
      arr = gets_bytestr(*args)
      arr.map { |msg| msg[:data] }.join
    end
    alias_method :gets_data_bytestr, :gets_data_s
    alias_method :gets_data_hex, :gets_data_s

    # Clears the input buffer
    # @return [Array]
    def clear_buffer
      @device.buffer.clear
    end

    # Gets any messages in the buffer in the same format as Input#gets, without removing them from the buffer
    # @param [*Object] args
    # @return [Array<Hash>]
    def gets_buffer(*args)
      @device.buffer
    end

    # Gets any messages in the buffer in the same format as Input#gets_s, without removing them from the buffer
    # @param [*Object] args
    # @return [Array<Hash>]
    def gets_buffer_s(*args)
      @device.buffer.map { |msg| msg[:data] = TypeConversion.numeric_byte_array_to_hex_string(msg[:data]); msg }
    end

    # Gets any messages in the buffer in the same format as Input#gets_data without removing them from the buffer
    # @param [*Object] args
    # @return [Array<Fixnum>]
    def gets_buffer_data(*args)
      @device.buffer.map { |msg| msg[:data] }
    end

  end

end
