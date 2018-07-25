# frozen_string_literal: true

module WebSocket
  module Handshake
    module Handler
      class Client11 < Client04
        private

        # @see WebSocket::Handshake::Handler::Base#handshake_keys
        def handshake_keys
          super.collect do |key_pair|
            if key_pair[0] == 'Sec-WebSocket-Origin'
              ['Origin', key_pair[1]]
            else
              key_pair
            end
          end
        end
      end
    end
  end
end
