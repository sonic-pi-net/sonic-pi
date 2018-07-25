# frozen_string_literal: true

require 'uri'

module WebSocket
  module Handshake
    # Construct or parse a client WebSocket handshake.
    #
    # @example
    #   @handshake = WebSocket::Handshake::Client.new(url: 'ws://example.com')
    #
    #   # Create request
    #   @handshake.to_s # GET /demo HTTP/1.1
    #                   # Upgrade: websocket
    #                   # Connection: Upgrade
    #                   # Host: example.com
    #                   # Origin: http://example.com
    #                   # Sec-WebSocket-Version: 13
    #                   # Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==
    #
    #   # Parse server response
    #   @handshake << <<EOF
    #   HTTP/1.1 101 Switching Protocols\r
    #   Upgrade: websocket\r
    #   Connection: Upgrade\r
    #   Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r
    #   \r
    #   EOF
    #
    #   # All data received?
    #   @handshake.finished?
    #
    #   # No parsing errors?
    #   @handshake.valid?
    #
    class Client < Base
      attr_reader :origin, :headers

      # Initialize new WebSocket Client
      #
      # @param [Hash] args Arguments for client
      #
      # @option args [String]         :host Host of request. Required if no :url param was provided.
      # @option args [String]         :origin Origin of request. Optional, should be used mostly by browsers. Default: nil
      # @option args [String]         :path Path of request. Should start with '/'. Default: '/'
      # @option args [Integer]        :port Port of request. Default: nil
      # @option args [String]         :query. Query for request. Should be in format "aaa=bbb&ccc=ddd"
      # @option args [Boolean]        :secure Defines protocol to use. If true then wss://, otherwise ws://. This option will not change default port - it should be handled by programmer.
      # @option args [String]         :url URL of request. Must by in format like ws://example.com/path?query=true. Every part of this url will be overriden by more specific arguments.
      # @option args [String]         :uri Alias to :url
      # @option args [Array<String>]  :protocols An array of supported sub-protocols
      # @option args [Integer]        :version Version of WebSocket to use. Default: 13 (this is version from RFC)
      # @option args [Hash]           :headers HTTP headers to use in the handshake
      #
      # @example
      #   Websocket::Handshake::Client.new(url: "ws://example.com/path?query=true")
      def initialize(args = {})
        super

        if @url || @uri
          uri = URI.parse(@url || @uri)
          @secure ||= (uri.scheme == 'wss')
          @host ||= uri.host
          @port ||= uri.port
          @path ||= uri.path
          @query ||= uri.query
        end

        @path = '/' if @path.nil? || @path.empty?
        @version ||= DEFAULT_VERSION

        raise WebSocket::Error::Handshake::NoHostProvided unless @host

        include_version
      end
      rescue_method :initialize

      # Add text of response from Server. This method will parse content immediately and update state and error(if neccessary)
      #
      # @param [String] data Data to add
      #
      # @example
      #   @handshake << <<EOF
      #   HTTP/1.1 101 Switching Protocols
      #   Upgrade: websocket
      #   Connection: Upgrade
      #   Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
      #
      #   EOF
      def <<(data)
        super
        parse_data
      end
      rescue_method :<<

      # Should send content to server after finished parsing?
      # @return [Boolean] false
      def should_respond?
        false
      end

      private

      # Include set of methods for selected protocol version
      # @return [Boolean] false if protocol number is unknown, otherwise true
      def include_version
        @handler = case @version
                   when 75 then Handler::Client75.new(self)
                   when 76, 0 then Handler::Client76.new(self)
                   when 1..3  then Handler::Client01.new(self)
                   when 4..10 then Handler::Client04.new(self)
                   when 11..17 then Handler::Client11.new(self)
                   else raise WebSocket::Error::Handshake::UnknownVersion
                   end
      end

      FIRST_LINE = %r{^HTTP\/1\.1 (\d{3})[\w\s]*$}

      # Parse first line of Server response.
      # @param [String] line Line to parse
      # @return [Boolean] True if parsed correctly. False otherwise
      def parse_first_line(line)
        line_parts = line.match(FIRST_LINE)
        raise WebSocket::Error::Handshake::InvalidHeader unless line_parts
        status = line_parts[1]
        raise WebSocket::Error::Handshake::InvalidStatusCode unless status == '101'
      end
    end
  end
end
