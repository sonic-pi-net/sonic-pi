# frozen_string_literal: true

require 'digest/md5'

module WebSocket
  module Handshake
    module Handler
      class Client01 < Client76
        private

        # @see WebSocket::Handshake::Handler::Base#handshake_keys
        def handshake_keys
          keys = super
          keys << ['Sec-WebSocket-Draft', @handshake.version]
          keys
        end
      end
    end
  end
end
