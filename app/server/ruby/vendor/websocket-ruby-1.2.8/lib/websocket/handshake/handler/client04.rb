# frozen_string_literal: true

require 'digest/sha1'
require 'base64'

module WebSocket
  module Handshake
    module Handler
      class Client04 < Client
        # @see WebSocket::Handshake::Base#valid?
        def valid?
          super && verify_accept && verify_protocol
        end

        private

        # @see WebSocket::Handshake::Handler::Base#handshake_keys
        def handshake_keys
          keys = [
            %w[Upgrade websocket],
            %w[Connection Upgrade]
          ]
          host = @handshake.host
          host += ":#{@handshake.port}" if @handshake.port
          keys << ['Host', host]
          keys += super
          keys << ['Sec-WebSocket-Origin', @handshake.origin] if @handshake.origin
          keys << ['Sec-WebSocket-Version', @handshake.version]
          keys << ['Sec-WebSocket-Key', key]
          keys << ['Sec-WebSocket-Protocol', @handshake.protocols.join(', ')] if @handshake.protocols.any?
          keys
        end

        # Sec-WebSocket-Key value
        # @return [String] key
        def key
          @key ||= Base64.encode64((1..16).map { rand(255).chr } * '').strip
        end

        # Value of Sec-WebSocket-Accept that should be delivered back by server
        # @return [Sering] accept
        def accept
          @accept ||= Base64.encode64(Digest::SHA1.digest(key + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11')).strip
        end

        # Verify if received header Sec-WebSocket-Accept matches generated one.
        # @return [Boolean] True if accept is matching. False otherwise(appropriate error is set)
        def verify_accept
          raise WebSocket::Error::Handshake::InvalidAuthentication unless @handshake.headers['sec-websocket-accept'] == accept
          true
        end

        def supported_protocols
          @handshake.protocols
        end

        def provided_protocols
          @handshake.headers['sec-websocket-protocol'].to_s.split(/ *, */)
        end
      end
    end
  end
end
