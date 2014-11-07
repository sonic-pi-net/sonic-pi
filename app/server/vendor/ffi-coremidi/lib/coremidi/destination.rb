module CoreMIDI

  # Output/Destination endpoint class
  class Destination

    include Endpoint

    attr_reader :entity

    # Close this output
    # @return [Boolean]
    def close
      if @enabled
        @enabled = false
        true
      else
        false
      end
    end

    # Send a MIDI message comprised of a String of hex digits
    # @param [String] data A string of hex digits eg "904040"
    # @return [Boolean]
    def puts_s(data)
      data = data.dup
      bytes = []
      until (str = data.slice!(0,2)).eql?("")
        bytes << str.hex
      end
      puts_bytes(*bytes)
      true
    end
    alias_method :puts_bytestr, :puts_s
    alias_method :puts_hex, :puts_s

    # Send a MIDI message comprised of numeric bytes
    # @param [*Fixnum] data Numeric bytes eg 0x90, 0x40, 0x40
    # @return [Boolean]
    def puts_bytes(*data)
      type = sysex?(data) ? :sysex : :small
      bytes = API.get_midi_packet(data)
      send("puts_#{type.to_s}", bytes, data.size)
      true
    end

    # Send a MIDI message of indeterminate type
    # @param [*Array<Fixnum>, *Array<String>, *Fixnum, *String] args
    # @return [Boolean]
    def puts(*args)
      case args.first
      when Array then args.each { |arg| puts(*arg) }
      when Fixnum then puts_bytes(*args)
      when String then puts_bytestr(*args)
      end
    end
    alias_method :write, :puts

    # Enable this device
    # @return [Destination]
    def enable(options = {}, &block)
      @enabled = true unless @enabled
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

    # Shortcut to the first output endpoint available
    # @return [Destination]
    def self.first
      Endpoint.first(:destination)
    end

    # Shortcut to the last output endpoint available
    # @return [Destination]
    def self.last
      Endpoint.last(:destination)
    end

    # All output endpoints
    # @return [Array<Destination>]
    def self.all
      Endpoint.all_by_type[:destination]
    end

    protected

    # Base initialization for this endpoint -- done whether or not the endpoint is enabled to
    # check whether it is truly available for use
    # @return [Boolean]
    def connect
      client_error = enable_client
      port_error = initialize_port
      @resource = API.MIDIEntityGetDestination( @entity.resource, @resource_id )
      !@resource.address.zero? && client_error.zero? && port_error.zero?
    end
    alias_method :connect?, :connect

    private

    # Output a short MIDI message
    def puts_small(bytes, size)
      packet_list = API.get_midi_packet_list(bytes, size)
      API.MIDISend(@handle, @resource, packet_list)
      true
    end

    # Output a System Exclusive MIDI message
    def puts_sysex(bytes, size)
      request = API::MIDISysexSendRequest.new
      request[:destination] = @resource
      request[:data] = bytes
      request[:bytes_to_send] = size
      request[:complete] = 0
      request[:completion_proc] = SysexCompletionCallback
      request[:completion_ref_con] = request
      API.MIDISendSysex(request)
      true
    end

    SysexCompletionCallback =
      API.get_callback([:pointer]) do |sysex_request_ptr|
      # this isn't working for some reason. as of now, it's not needed though
      end

    # Initialize a coremidi port for this endpoint
    def initialize_port
      port = API.create_midi_output_port(@client, @resource_id, @name)
      @handle = port[:handle]
      port[:error]
    end

    # Is the given data a MIDI sysex message?
    def sysex?(data)
      data.first.eql?(0xF0) && data.last.eql?(0xF7)
    end

  end

end
