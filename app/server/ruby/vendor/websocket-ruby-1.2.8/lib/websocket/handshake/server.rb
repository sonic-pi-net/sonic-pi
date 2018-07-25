# frozen_string_literal: true

module WebSocket
  module Handshake
    # Construct or parse a server WebSocket handshake.
    #
    # @example
    #   handshake = WebSocket::Handshake::Server.new
    #
    #   # Parse client request
    #   @handshake << <<EOF
    #   GET /demo HTTP/1.1\r
    #   Upgrade: websocket\r
    #   Connection: Upgrade\r
    #   Host: example.com\r
    #   Origin: http://example.com\r
    #   Sec-WebSocket-Version: 13\r
    #   Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r
    #   \r
    #   EOF
    #
    #   # All data received?
    #   @handshake.finished?
    #
    #   # No parsing errors?
    #   @handshake.valid?
    #
    #   # Create response
    #   @handshake.to_s # HTTP/1.1 101 Switching Protocols
    #                   # Upgrade: websocket
    #                   # Connection: Upgrade
    #                   # Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
    #
    class Server < Base
      # Initialize new WebSocket Server
      #
      # @param [Hash] args Arguments for server
      #
      # @option args [Boolean] :secure If true then server will use wss:// protocol
      # @option args [Array<String>] :protocols an array of supported sub-protocols
      #
      # @example
      #   Websocket::Handshake::Server.new(secure: true)
      def initialize(args = {})
        super
        @secure ||= false
      end

      # Add text of request from Client. This method will parse content immediately and update version, state and error(if neccessary)
      #
      # @param [String] data Data to add
      #
      # @example
      #   @handshake << <<EOF
      #   GET /demo HTTP/1.1
      #   Upgrade: websocket
      #   Connection: Upgrade
      #   Host: example.com
      #   Origin: http://example.com
      #   Sec-WebSocket-Version: 13
      #   Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==
      #
      #   EOF
      def <<(data)
        super
        set_version if parse_data
      end
      rescue_method :<<

      # Parse the request from a rack environment
      # @param env Rack Environment
      #
      # @example
      #   @handshake.from_rack(env)
      def from_rack(env)
        @headers = env.select { |key, _value| key.to_s.start_with? 'HTTP_' }.each_with_object({}) do |tuple, memo|
          key, value = tuple
          memo[key.gsub(/\AHTTP_/, '').tr('_', '-').downcase] = value
        end

        @path      = env['REQUEST_PATH']
        @query     = env['QUERY_STRING']

        set_version

        # Passenger is blocking on read
        # Unicorn doesn't support readpartial
        # Maybe someone is providing even plain string?
        # Better safe than sorry...
        if @version == 76
          input = env['rack.input']
          @leftovers = if input.respond_to?(:readpartial)
                         input.readpartial
                       elsif input.respond_to?(:read)
                         input.read
                       else
                         input.to_s
                       end
        end

        @state = :finished
      end

      # Parse the request from hash
      # @param hash Hash to import data
      # @option hash [Hash] :headers HTTP headers of request, downcased
      # @option hash [String] :path Path for request(without host and query string)
      # @option hash [String] :query Query string for request
      # @option hash [String] :body Body of request(if exists)
      #
      # @example
      #   @handshake.from_hash(hash)
      def from_hash(hash)
        @headers = hash[:headers] || {}
        @path = hash[:path] || '/'
        @query = hash[:query] || ''
        @leftovers = hash[:body]

        set_version
        @state = :finished
      end

      # Should send content to client after finished parsing?
      # @return [Boolean] true
      def should_respond?
        true
      end

      # Host of server according to client header
      # @return [String] host
      def host
        @headers['host'].to_s.split(':')[0].to_s
      end

      # Port of server according to client header
      # @return [String] port
      def port
        @headers['host'].to_s.split(':')[1]
      end

      private

      # Set version of protocol basing on client requets. AFter cotting method calls include_version.
      def set_version
        @version = @headers['sec-websocket-version'].to_i if @headers['sec-websocket-version']
        @version ||= @headers['sec-websocket-draft'].to_i if @headers['sec-websocket-draft']
        @version ||= 76 if @headers['sec-websocket-key1']
        @version ||= 75
        include_version
      end

      # Include set of methods for selected protocol version
      # @return [Boolean] false if protocol number is unknown, otherwise true
      def include_version
        @handler = case @version
                   when 75 then Handler::Server75.new(self)
                   when 76, 0..3 then Handler::Server76.new(self)
                   when 4..17 then Handler::Server04.new(self)
                   else raise WebSocket::Error::Handshake::UnknownVersion
                   end
      end

      PATH = %r{^(\w+) (\/[^\s]*) HTTP\/1\.1$}

      # Parse first line of Client response.
      # @param [String] line Line to parse
      # @return [Boolean] True if parsed correctly. False otherwise
      def parse_first_line(line)
        line_parts = line.match(PATH)
        raise WebSocket::Error::Handshake::InvalidHeader unless line_parts
        method = line_parts[1].strip
        raise WebSocket::Error::Handshake::GetRequestRequired unless method == 'GET'

        resource_name = line_parts[2].strip
        @path, @query = resource_name.split('?', 2)
      end
    end
  end
end
