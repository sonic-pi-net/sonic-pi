# frozen_string_literal: true

module WebSocket
  module Handshake
    module Handler
      # This class and it's descendants are included in client or server handshake in order to extend basic functionality
      class Base
        def initialize(handshake)
          @handshake = handshake
        end

        # @see WebSocket::Handshake::Base#to_s
        def to_s
          result = [header_line]
          handshake_keys.each do |key|
            result << key.join(': ')
          end
          result << ''
          result << finishing_line
          result.join("\r\n")
        end

        def valid?
          true
        end

        private

        # Set first line of text representation according to specification.
        # @return [String] First line of HTTP header
        def header_line
          ''
        end

        # Set handshake headers. Provided as array because some protocol version require specific order of fields.
        # @return [Array] List of headers as arrays [key, value]
        def handshake_keys
          []
        end

        # Set data to send after headers. In most cases it will be blank data.
        # @return [String] data
        def finishing_line
          ''
        end
      end
    end
  end
end
