# frozen_string_literal: true

require 'digest/md5'

module WebSocket
  module Handshake
    module Handler
      class Server76 < Server75
        # @see WebSocket::Handshake::Base#valid?
        def valid?
          super && !finishing_line.nil?
        end

        private

        def headers
          {
            origin: 'Sec-WebSocket-Origin',
            location: 'Sec-WebSocket-Location',
            protocol: 'Sec-WebSocket-Protocol'
          }.freeze
        end

        # @see WebSocket::Handshake::Base#reserved_leftover_lines
        def reserved_leftover_lines
          1
        end

        # @see WebSocket::Handshake::Handler::Base#header_line
        def header_line
          'HTTP/1.1 101 WebSocket Protocol Handshake'
        end

        # @see WebSocket::Handshake::Handler::Base#finishing_line
        def finishing_line
          @finishing_line ||= challenge_response
        end

        # Response to client challenge from request Sec-WebSocket-Key1, Sec-WebSocket-Key2 and leftovers
        # @return [String] Challenge response or nil if error occured
        def challenge_response
          # Refer to 5.2 4-9 of the draft 76
          first = numbers_over_spaces(@handshake.headers['sec-websocket-key1'].to_s)
          second = numbers_over_spaces(@handshake.headers['sec-websocket-key2'].to_s)
          third = @handshake.leftovers

          sum = [first].pack('N*') +
                [second].pack('N*') +
                third
          Digest::MD5.digest(sum)
        end

        # Calculate numbers over spaces, according to spec 5.2
        # @param [String] string Key to parse
        # @return [Integer] Result of calculations or nil if error occured
        def numbers_over_spaces(string)
          numbers = string.scan(/[0-9]/).join.to_i

          spaces = string.scan(/ /).size
          # As per 5.2.5, abort the connection if spaces are zero.
          raise WebSocket::Error::Handshake::InvalidAuthentication if spaces.zero?

          # As per 5.2.6, abort if numbers is not an integral multiple of spaces
          raise WebSocket::Error::Handshake::InvalidAuthentication if numbers % spaces != 0

          quotient = numbers / spaces

          raise WebSocket::Error::Handshake::InvalidAuthentication if quotient > 2**32 - 1

          quotient
        end
      end
    end
  end
end
