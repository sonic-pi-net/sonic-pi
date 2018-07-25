# frozen_string_literal: true

module WebSocket
  module Handshake
    module Handler
      class Client75 < Client
        # @see WebSocket::Handshake::Base#valid?
        def valid?
          super && verify_protocol
        end

        private

        # @see WebSocket::Handshake::Handler::Base#handshake_keys
        def handshake_keys
          keys = [
            %w[Upgrade WebSocket],
            %w[Connection Upgrade]
          ]
          host = @handshake.host
          host += ":#{@handshake.port}" if @handshake.port
          keys << ['Host', host]
          keys << ['Origin', @handshake.origin] if @handshake.origin
          keys << ['WebSocket-Protocol', @handshake.protocols.first] if @handshake.protocols.any?
          keys += super
          keys
        end

        def supported_protocols
          Array(@handshake.protocols.first)
        end

        def provided_protocols
          Array(@handshake.headers['websocket-protocol'].to_s.strip)
        end
      end
    end
  end
end
