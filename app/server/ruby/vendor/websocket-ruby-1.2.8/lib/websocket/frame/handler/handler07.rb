# encoding: binary
# frozen_string_literal: true

module WebSocket
  module Frame
    module Handler
      class Handler07 < Handler05
        # Hash of frame names and it's opcodes
        FRAME_TYPES = {
          continuation: 0,
          text: 1,
          binary: 2,
          close: 8,
          ping: 9,
          pong: 10
        }.freeze

        # Hash of frame opcodes and it's names
        FRAME_TYPES_INVERSE = FRAME_TYPES.invert.freeze

        def encode_frame
          if @frame.type == :close
            code = @frame.code || 1000
            raise WebSocket::Error::Frame::UnknownCloseCode unless valid_code?(code)
            @frame.data = Data.new([code].pack('n') + @frame.data.to_s)
            @frame.code = nil
          end
          super
        end

        def decode_frame
          result = super
          if close_code?(result)
            code = result.data.slice!(0..1)
            result.code = code.unpack('n').first
            raise WebSocket::Error::Frame::UnknownCloseCode unless valid_code?(result.code)
            raise WebSocket::Error::Frame::InvalidPayloadEncoding unless valid_encoding?(result.data)
          end
          result
        end

        private

        def valid_code?(code)
          [1000, 1001, 1002, 1003, 1007, 1008, 1009, 1010, 1011].include?(code) || (3000..4999).cover?(code)
        end

        def valid_encoding?(data)
          return true if data.nil?
          data.encode('UTF-8')
          true
        rescue StandardError
          false
        end

        def close_code?(frame)
          frame && frame.type == :close && !frame.data.empty?
        end

        # Convert frame type name to opcode
        # @param [Symbol] frame_type Frame type name
        # @return [Integer] opcode or nil
        # @raise [WebSocket::Error] if frame opcode is not known
        def type_to_opcode(frame_type)
          FRAME_TYPES[frame_type] || raise(WebSocket::Error::Frame::UnknownFrameType)
        end

        # Convert frame opcode to type name
        # @param [Integer] opcode Opcode
        # @return [Symbol] Frame type name or nil
        # @raise [WebSocket::Error] if frame type name is not known
        def opcode_to_type(opcode)
          FRAME_TYPES_INVERSE[opcode] || raise(WebSocket::Error::Frame::UnknownOpcode)
        end
      end
    end
  end
end
