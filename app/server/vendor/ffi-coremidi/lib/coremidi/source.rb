module CoreMIDI

  # Input/Source endpoint class
  class Source

    include Endpoint

    attr_reader :buffer

    #
    # An array of MIDI event hashes as such:
    #   [
    #     { :data => [144, 60, 100], :timestamp => 1024 },
    #     { :data => [128, 60, 100], :timestamp => 1100 },
    #     { :data => [144, 40, 120], :timestamp => 1200 }
    #   ]
    #
    # The data is an array of Numeric bytes
    # The timestamp is the number of millis since this input was enabled
    #
    # @return [Array<Hash>]
    def gets
      until queued_messages?
        # per https://github.com/arirusso/unimidi/issues/20#issuecomment-44761318
        sleep(0.0001) # patch to prevent 100% CPU issue with some midi controllers
      end
      messages = queued_messages
      @pointer = @buffer.length
      messages
    end
    alias_method :read, :gets

    # Same as Source#gets except that it returns message data as string of hex 
    # digits as such:
    #   [
    #     { :data => "904060", :timestamp => 904 },
    #     { :data => "804060", :timestamp => 1150 },
    #     { :data => "90447F", :timestamp => 1300 }
    #   ]
    #
    # @return [Array<Hash>]
    def gets_s
      messages = gets
      messages.each do |message| 
        message[:data] = TypeConversion.numeric_bytes_to_hex_string(message[:data])
      end
      messages
    end
    alias_method :gets_bytestr, :gets_s

    # Enable this the input for use; can be passed a block
    # @return [Source]
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

    # Close this input
    # @return [Boolean]
    def close
      #error = API.MIDIPortDisconnectSource( @handle, @resource )
      #raise "MIDIPortDisconnectSource returned error code #{error}" unless error.zero?
      #error = API.MIDIClientDispose(@handle)
      #raise "MIDIClientDispose returned error code #{error}" unless error.zero?
      #error = API.MIDIPortDispose(@handle)
      #raise "MIDIPortDispose returned error code #{error}" unless error.zero?
      #error = API.MIDIEndpointDispose(@resource)
      #raise "MIDIEndpointDispose returned error code #{error}" unless error.zero?
      if @enabled
        @enabled = false
        true
      else
        false
      end
    end

    # Shortcut to the first available input endpoint
    # @return [Source]
    def self.first
      Endpoint.first(:source)
    end

    # Shortcut to the last available input endpoint
    # @return [Source]
    def self.last
      Endpoint.last(:source)
    end

    # All input endpoints
    # @return [Array<Source>]
    def self.all
      Endpoint.all_by_type[:source]
    end

    protected

    # Base initialization for this endpoint -- done whether or not the endpoint is enabled to check whether 
    # it is truly available for use
    def connect   
      enable_client
      initialize_port
      @resource = API.MIDIEntityGetSource(@entity.resource, @resource_id)
      error = API.MIDIPortConnectSource(@handle, @resource, nil )
      initialize_buffer
      @sysex_buffer = []
      @start_time = Time.now.to_f

      error.zero?
    end
    alias_method :connect?, :connect

    private

    # Add a single message to the buffer
    # @param [Array<Fixnum>] bytes Message data
    # @param [Float] timestamp The system float timestamp
    # @return [Array<Hash>] The resulting buffer
    def enqueue_message(bytes, timestamp)
      if bytes.first.eql?(0xF0) || !@sysex_buffer.empty?
        @sysex_buffer += bytes
        if bytes.last.eql?(0xF7)
          bytes = @sysex_buffer.dup
          @sysex_buffer.clear
        end
      end
      @buffer << get_message_formatted(bytes, timestamp) if @sysex_buffer.empty? 
      @buffer
    end

    # New MIDI messages from the queue
    def queued_messages
      @buffer.slice(@pointer, @buffer.length - @pointer)
    end

    # Are there new MIDI messages in the queue?
    def queued_messages?
      @pointer < @buffer.length
    end

    # The callback fired by coremidi when new MIDI messages are in the buffer
    def get_event_callback
      Proc.new do |new_packets, refCon_ptr, connRefCon_ptr|
        begin
          # p "packets received: #{new_packets[:numPackets]}"
          timestamp = Time.now.to_f
          messages = get_messages(new_packets)
          messages.each { |message| enqueue_message(message, timestamp) }
        rescue Exception => exception
          Thread.main.raise(exception)
        end
      end
    end

    # Get MIDI messages from the given CoreMIDI packet list
    # @param [API::MIDIPacketList] new_packets The packet list
    # @return [Array<Array<Fixnum>>] A collection of MIDI messages
    def get_messages(packet_list)
      count = packet_list[:numPackets]
      first = packet_list[:packet][0]
      data = first[:data].to_a
      messages = []
      messages << data.slice!(0, first[:length])
      (count - 1).times do |i|
        length_index = find_next_length_index(data)
        message_length = data[length_index]
        unless message_length.nil?
          packet_start_index = length_index + 2
          packet_end_index = packet_start_index + message_length
          if data.length >= packet_end_index + 1
            packet = data.slice!(0..packet_end_index)
            message = packet.slice(packet_start_index, message_length)
            messages << message
          end
        end
      end
      messages
    end

    # Get the next index for "length" from the blob of MIDI data
    # @param [Array<Fixnum>] data
    # @return [Fixnum]
    def find_next_length_index(data)
      last_is_zero = false
      data.each_with_index do |num, i|
        if num.zero?
          if last_is_zero
            return i + 1
          else
            last_is_zero = true
          end
        else
          last_is_zero = false
        end
      end
    end

    # Timestamp for a received MIDI message
    # @return [Fixnum]
    def timestamp(now)
      (now - @start_time) * 1000
    end

    # Give a message its timestamp and package it in a Hash
    # @return [Hash]
    def get_message_formatted(raw, time)
      { 
        :data => raw, 
        :timestamp => timestamp(time) 
      }
    end

    # Initialize a coremidi port for this endpoint
    # @return [Boolean]
    def initialize_port
      @callback = get_event_callback
      port = API.create_midi_input_port(@client, @resource_id, @name, @callback)
      @handle = port[:handle]
      raise "MIDIInputPortCreate returned error code #{port[:error]}" unless port[:error].zero?
      true
    end

    # Initialize the MIDI message buffer
    # @return [Boolean]
    def initialize_buffer
      @pointer = 0
      @buffer = []
      def @buffer.clear
        super
        @pointer = 0
      end
      true
    end

  end

end
