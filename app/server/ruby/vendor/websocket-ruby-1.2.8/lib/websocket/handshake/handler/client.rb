# frozen_string_literal: true

module WebSocket
  module Handshake
    module Handler
      class Client < Base
        private

        # @see WebSocket::Handshake::Handler::Base#header_line
        def header_line
          path = @handshake.path
          path += '?' + @handshake.query if @handshake.query
          "GET #{path} HTTP/1.1"
        end

        # @see WebSocket::Handshake::Handler::Base#header_handshake_keys
        def handshake_keys
          super + @handshake.headers.to_a
        end

        # Verify if received header matches with one of the sent ones
        # @return [Boolean] True if matching. False otherwise(appropriate error is set)
        def verify_protocol
          return true if supported_protocols.empty?
          protos = provided_protocols & supported_protocols
          raise WebSocket::Error::Handshake::UnsupportedProtocol if protos.empty?
          true
        end
      end
    end
  end
end
