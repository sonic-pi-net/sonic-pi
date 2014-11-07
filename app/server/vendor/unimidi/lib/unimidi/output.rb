module UniMIDI

  # A MIDI output device
  class Output

    extend Device::ClassMethods
    include Device::InstanceMethods

    # All MIDI output devices -- used to populate the class
    # @return [Array<Output>]
    def self.all
      Loader.devices(:direction => :output)
    end

    # Sends a message to the output. 
    #
    # The message format can be:
    #
    # 1. Numeric bytes eg output.puts(0x90, 0x40, 0x40)
    # 2. An array of numeric bytes [0x90, 0x40, 0x40]
    # 3. A string of bytes eg "904040"
    # 4. An array of strings ["904040", "804040"]
    #
    # @param [*Array<Fixnum>, *Array<String>, *Fixnum, *String] messages
    # @return [Array<Fixnum>, Array<String>]
    def puts(*messages)
      message = messages.first
      case message
      when Array then messages.each { |array| puts(*array.flatten) }
      when Fixnum then puts_bytes(*messages)
      when String then puts_s(*messages)
      else
        if message.respond_to?(:to_bytes)
          puts_bytes(*message.to_bytes.flatten)
        elsif message.respond_to?(:to_a) 
          puts_bytes(*message.to_a.flatten)
        end
      end
    end

    # Sends a message to the output in a form of a string eg "904040".  This method does not do
    # type checking
    # @param [*String] messages
    # @return [Array<String>, Array<Array<String>>]
    def puts_s(*messages)
      @device.puts_s(*messages)
      messages.count < 2 ? messages[0] : messages
    end
    alias_method :puts_bytestr, :puts_s
    alias_method :puts_hex, :puts_s

    # Sends a message to the output in a form of bytes eg output.puts_bytes(0x90, 0x40, 0x40).
    # This method does not do type checking.
    # @param [*Array<Fixnum>] messages
    # @return [Array<Fixnum>, Array<Array<Fixnum>>]
    def puts_bytes(*messages)
      @device.puts_bytes(*messages)
      messages.count < 2 ? messages[0] : messages
    end

  end

end
