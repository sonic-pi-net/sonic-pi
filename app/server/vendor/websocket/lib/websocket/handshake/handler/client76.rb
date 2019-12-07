require 'digest/md5'

module WebSocket
  module Handshake
    module Handler
      class Client76 < Client75

        # @see WebSocket::Handshake::Base#valid?
        def valid?
          super && verify_challenge
        end

        private

        # @see WebSocket::Handshake::Base#reserved_leftover_lines
        def reserved_leftover_lines
          1
        end

        # @see WebSocket::Handshake::Handler::Base#handshake_keys
        def handshake_keys
          keys = super
          keys << ['Sec-WebSocket-Key1', key1]
          keys << ['Sec-WebSocket-Key2', key2]
          keys
        end

        # @see WebSocket::Handshake::Handler::Base#finishing_line
        def finishing_line
          key3
        end

        # Sec-WebSocket-Key1 value
        # @return [String] key
        def key1
          @key1 ||= generate_key(:key1)
        end

        # Sec-WebSocket-Key2 value
        # @return [String] key
        def key2
          @key2 ||= generate_key(:key2)
        end

        # Value of third key, sent in body
        # @return [String] key
        def key3
          @key3 ||= generate_key3
        end

        # Expected challenge that should be sent by server
        # @return [String] challenge
        def challenge
          return @challenge if defined?(@challenge)
          key1 && key2
          sum = [@key1_number].pack("N*") +
                [@key2_number].pack("N*") +
                key3

          @challenge = Digest::MD5.digest(sum)
        end

        # Verify if challenge sent by server match generated one
        # @return [Boolena] True if challenge matches, false otherwise(sets appropriate error)
        def verify_challenge
          raise WebSocket::Error::Handshake::InvalidAuthentication unless @handshake.instance_variable_get('@leftovers') == challenge
          true
        end

        NOISE_CHARS = ("\x21".."\x2f").to_a() + ("\x3a".."\x7e").to_a()

        # Generate Sec-WebSocket-Key1 and Sec-WebSocket-Key2
        # @param [String] name of key. Will be used to set number variable needed later. Valid values: key1, key2
        # @return [String] generated key
        def generate_key(key)
          spaces = 1 + rand(12)
          max = 0xffffffff / spaces
          number = rand(max + 1)
          instance_variable_set("@#{key}_number", number)
          key = (number * spaces).to_s
          (1 + rand(12)).times() do
            char = NOISE_CHARS[rand(NOISE_CHARS.size)]
            pos = rand(key.size + 1)
            key[pos...pos] = char
          end
          spaces.times() do
            pos = 1 + rand(key.size - 1)
            key[pos...pos] = " "
          end
          return key
        end

        # Generate third key
        def generate_key3
          [rand(0x100000000)].pack("N") + [rand(0x100000000)].pack("N")
        end

      end
    end
  end
end
