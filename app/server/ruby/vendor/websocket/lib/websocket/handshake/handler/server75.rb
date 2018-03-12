module WebSocket
  module Handshake
    module Handler
      class Server75 < Server

        private

        # @see WebSocket::Handshake::Handler::Base#header_line
        def header_line
          "HTTP/1.1 101 Web Socket Protocol Handshake"
        end

        # @see WebSocket::Handshake::Handler::Base#handshake_keys
        def handshake_keys
          [
            ["Upgrade", "WebSocket"],
            ["Connection", "Upgrade"],
            ["WebSocket-Origin", @handshake.headers['origin']],
            ["WebSocket-Location", @handshake.uri]
          ]
        end

      end
    end
  end
end
