# frozen_string_literal: true

module WebSocket
  module Handshake
    module Handler
      class Server75 < Server
        private

        def headers
          {
            origin: 'WebSocket-Origin',
            location: 'WebSocket-Location',
            protocol: 'WebSocket-Protocol'
          }.freeze
        end

        # @see WebSocket::Handshake::Handler::Base#header_line
        def header_line
          'HTTP/1.1 101 Web Socket Protocol Handshake'
        end

        # @see WebSocket::Handshake::Handler::Base#handshake_keys
        def handshake_keys
          [
            %w[Upgrade WebSocket],
            %w[Connection Upgrade],
            [headers[:origin], @handshake.headers['origin']],
            [headers[:location], @handshake.uri]
          ] + protocol
        end

        def protocol
          return [] unless @handshake.headers.key?(headers[:protocol].downcase)
          proto = @handshake.headers[headers[:protocol].downcase]
          [[headers[:protocol], @handshake.protocols.include?(proto) ? proto : nil]]
        end
      end
    end
  end
end
