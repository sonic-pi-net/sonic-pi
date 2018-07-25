# frozen_string_literal: true

require 'digest/sha1'
require 'base64'

module WebSocket
  module Handshake
    module Handler
      class Server04 < Server
        # @see WebSocket::Handshake::Base#valid?
        def valid?
          super && verify_key
        end

        private

        # @see WebSocket::Handshake::Handler::Base#header_line
        def header_line
          'HTTP/1.1 101 Switching Protocols'
        end

        # @see WebSocket::Handshake::Handler::Base#handshake_keys
        def handshake_keys
          [
            %w[Upgrade websocket],
            %w[Connection Upgrade],
            ['Sec-WebSocket-Accept', signature]
          ] + protocol
        end

        # Signature of response, created from client request Sec-WebSocket-Key
        # @return [String] signature
        def signature
          return unless key
          string_to_sign = "#{key}258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
          Base64.encode64(Digest::SHA1.digest(string_to_sign)).chomp
        end

        def verify_key
          raise WebSocket::Error::Handshake::InvalidAuthentication unless key
          true
        end

        def key
          @handshake.headers['sec-websocket-key']
        end

        def protocol
          return [] unless @handshake.headers.key?('sec-websocket-protocol')
          protos = @handshake.headers['sec-websocket-protocol'].split(/ *, */) & @handshake.protocols
          [['Sec-WebSocket-Protocol', protos.first]]
        end
      end
    end
  end
end
