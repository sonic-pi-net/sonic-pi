# frozen_string_literal: true

module WebSocket
  module Handshake
    # @abstract Subclass and override to implement custom handshakes
    class Base
      include ExceptionHandler
      include NiceInspect

      attr_reader :host, :port, :path, :query,
                  :state, :version, :secure,
                  :headers, :protocols

      # Initialize new WebSocket Handshake and set it's state to :new
      def initialize(args = {})
        args.each do |k, v|
          value = begin
            v.dup
          rescue TypeError
            v
          end
          instance_variable_set("@#{k}", value)
        end

        @state = :new
        @handler = nil

        @data = String.new('')
        @headers ||= {}
        @protocols ||= []
      end

      # @abstract Add data to handshake
      def <<(data)
        @data << data
      end

      # Return textual representation of handshake request or response
      # @return [String] text of response
      def to_s
        @handler ? @handler.to_s : ''
      end
      rescue_method :to_s, return: ''

      # Is parsing of data finished?
      # @return [Boolena] True if request was completely parsed or error occured. False otherwise
      def finished?
        @state == :finished || @state == :error
      end

      # Is parsed data valid?
      # @return [Boolean] False if some errors occured. Reason for error could be found in error method
      def valid?
        finished? && @error.nil? && @handler && @handler.valid?
      end
      rescue_method :valid?, return: false

      # @abstract Should send data after parsing is finished?
      def should_respond?
        raise NotImplementedError
      end

      # Data left from parsing. Sometimes data that doesn't belong to handshake are added - use this method to retrieve them.
      # @return [String] String if some data are available. Nil otherwise
      def leftovers
        (@leftovers.to_s.split("\n", reserved_leftover_lines + 1)[reserved_leftover_lines] || '').strip
      end

      # URI of request.
      # @return [String] Full URI with protocol
      # @example
      #   @handshake.uri #=> "ws://example.com/path?query=true"
      def uri
        uri =  String.new(secure ? 'wss://' : 'ws://')
        uri << host
        uri << ":#{port}" if port
        uri << path
        uri << "?#{query}" if query
        uri
      end

      private

      # Number of lines after header that should be handled as belonging to handshake. Any data after those lines will be handled as leftovers.
      # @return [Integer] Number of lines
      def reserved_leftover_lines
        0
      end

      # Changes state to error and sets error message
      # @param [String] message Error message to set
      def error=(message)
        @state = :error
        super
      end

      HEADER = /^([^:]+):\s*(.+)$/

      # Parse data imported to handshake and sets state to finished if necessary.
      # @return [Boolean] True if finished parsing. False if not all data received yet.
      def parse_data
        header, @leftovers = @data.split("\r\n\r\n", 2)
        return false unless @leftovers # The whole header has not been received yet.

        lines = header.split("\r\n")

        first_line = lines.shift
        parse_first_line(first_line)

        lines.each do |line|
          h = HEADER.match(line)
          next unless h # Skip any invalid headers
          key = h[1].strip.downcase
          val = h[2].strip
          # If the header is already set and refers to the websocket protocol, append the new value
          if @headers.key?(key) && key =~ /^(sec-)?websocket-protocol$/
            @headers[key] << ", #{val}"
          else
            @headers[key] = val
          end
        end

        @state = :finished
        true
      end
    end
  end
end
