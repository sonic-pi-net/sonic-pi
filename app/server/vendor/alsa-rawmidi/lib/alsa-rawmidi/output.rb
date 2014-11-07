module AlsaRawMIDI

  # Output device class
  class Output

    include Device

    # Close this output
    # @return [Boolean]
    def close
      if @enabled
        API::Device.close(@resource)
        @enabled = false
        true
      else
        false
      end
    end

    # Output a MIDI message in hex string format
    # @param [String] data
    # @return [Boolean]
    def puts_s(data)
      data = data.dup
      output = []
      until (str = data.slice!(0,2)) == ""
        output << str.hex
      end
      puts_bytes(*output)
      true
    end
    alias_method :puts_bytestr, :puts_s
    alias_method :puts_hex, :puts_s

    # Output a MIDI message in numeric byte format
    # @param [*Fixnum] data
    # @return [Boolean]
    def puts_bytes(*data)
      API::Output.puts(@resource, data)
      true
    end

    # Output the given MIDI message
    # @param [*Fixnum, *String] args
    # @return [Boolean]
    def puts(*args)
      case args.first
        when Array then args.each { |arg| puts(*arg) }
        when Numeric then puts_bytes(*args)
        when String then puts_bytestr(*args)
      end
    end
    alias_method :write, :puts

    # Enable this device; yields
    # @param [Hash] options
    # @param [Proc] block
    # @return [Output]
    def enable(options = {}, &block)
      unless @enabled
        @resource = API::Output.open(@system_id)
        @enabled = true
      end
      if block_given?
        begin
          yield(self)
        ensure
          close
        end
      end
      self
    end
    alias_method :open, :enable
    alias_method :start, :enable

    # The first available output
    # @return [Output]
    def self.first
      Device.first(:output)
    end

    # The last available output
    # @return [Output]
    def self.last
      Device.last(:output)
    end

    # All outputs
    # @return [Array<Output>]
    def self.all
      Device.all_by_type[:output]
    end

  end

end
