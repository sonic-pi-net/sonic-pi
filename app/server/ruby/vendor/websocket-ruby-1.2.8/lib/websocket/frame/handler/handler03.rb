# encoding: binary
# frozen_string_literal: true

require 'securerandom'

module WebSocket
  module Frame
    module Handler
      class Handler03 < Base
        # Hash of frame names and it's opcodes
        FRAME_TYPES = {
          continuation: 0,
          close: 1,
          ping: 2,
          pong: 3,
          text: 4,
          binary: 5
        }.freeze

        # Hash of frame opcodes and it's names
        FRAME_TYPES_INVERSE = FRAME_TYPES.invert.freeze

        # @see WebSocket::Frame::Base#supported_frames
        def supported_frames
          %i[text binary close ping pong]
        end

        # @see WebSocket::Frame::Handler::Base#encode_frame
        def encode_frame
          frame = if @frame.outgoing_masking?
                    masking_key = SecureRandom.random_bytes(4)
                    tmp_data = Data.new(masking_key + @frame.data)
                    tmp_data.set_mask
                    masking_key + tmp_data.getbytes(4, tmp_data.size)
                  else
                    @frame.data
                  end

          encode_header + frame
        end

        # @see WebSocket::Frame::Handler::Base#decode_frame
        def decode_frame
          while @frame.data.size > 1
            valid_header, more, frame_type, mask, payload_length = decode_header
            return unless valid_header

            application_data = decode_payload(payload_length, mask)

            if more
              decode_continuation_frame(application_data, frame_type)
            elsif frame_type == :continuation
              return decode_finish_continuation_frame(application_data)
            else
              raise(WebSocket::Error::Frame::InvalidPayloadEncoding) if frame_type == :text && !application_data.valid_encoding?
              return @frame.class.new(version: @frame.version, type: frame_type, data: application_data, decoded: true)
            end
          end
          nil
        end

        # Allow turning on or off masking
        def masking?
          false
        end

        private

        # This allows flipping the more bit to fin for draft 04
        def fin
          false
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

        def encode_header
          mask = @frame.outgoing_masking? ? 0b10000000 : 0b00000000

          output = String.new('')
          output << (type_to_opcode(@frame.type) | (fin ? 0b10000000 : 0b00000000)) # since more, rsv1-3 are 0 and 0x80 for Draft 4
          output << encode_payload_length(@frame.data.size, mask)
          output
        end

        def encode_payload_length(length, mask)
          output = String.new('')
          if length <= 125
            output << (length | mask) # since rsv4 is 0
          elsif length < 65_536 # write 2 byte length
            output << (126 | mask)
            output << [length].pack('n')
          else # write 8 byte length
            output << (127 | mask)
            output << [length >> 32, length & 0xFFFFFFFF].pack('NN')
          end
          output
        end

        def decode_header
          more, frame_type = decode_first_byte
          header_length, payload_length, mask = decode_second_byte(frame_type)
          return unless header_length

          # Compute the expected frame length
          frame_length = header_length + payload_length
          frame_length += 4 if mask

          raise(WebSocket::Error::Frame::TooLong) if frame_length > WebSocket.max_frame_size

          # Check buffer size
          return unless buffer_exists?(frame_length) # Buffer incomplete

          # Remove frame header
          @frame.data.slice!(0...header_length)

          [true, more, frame_type, mask, payload_length]
        end

        def buffer_exists?(buffer_number)
          !@frame.data.getbyte(buffer_number - 1).nil?
        end

        def decode_first_byte
          first_byte = @frame.data.getbyte(0)

          raise(WebSocket::Error::Frame::ReservedBitUsed) if first_byte & 0b01110000 != 0b00000000

          more = ((first_byte & 0b10000000) == 0b10000000) ^ fin
          frame_type = opcode_to_type first_byte & 0b00001111

          raise(WebSocket::Error::Frame::FragmentedControlFrame) if more && control_frame?(frame_type)
          raise(WebSocket::Error::Frame::DataFrameInsteadContinuation) if data_frame?(frame_type) && !@application_data_buffer.nil?

          [more, frame_type]
        end

        def decode_second_byte(frame_type)
          second_byte = @frame.data.getbyte(1)

          mask = @frame.incoming_masking? && (second_byte & 0b10000000) == 0b10000000
          length = second_byte & 0b01111111

          raise(WebSocket::Error::Frame::ControlFramePayloadTooLong) if length > 125 && control_frame?(frame_type)

          header_length, payload_length = decode_payload_length(length)

          [header_length, payload_length, mask]
        end

        def decode_payload_length(length)
          case length
          when 127 # Length defined by 8 bytes
            # Check buffer size
            return unless buffer_exists?(10) # Buffer incomplete

            # Only using the last 4 bytes for now, till I work out how to
            # unpack 8 bytes. I'm sure 4GB frames will do for now :)
            [10, @frame.data.getbytes(6, 4).unpack('N').first]
          when 126 # Length defined by 2 bytes
            # Check buffer size
            return unless buffer_exists?(4) # Buffer incomplete

            [4, @frame.data.getbytes(2, 2).unpack('n').first]
          else
            [2, length]
          end
        end

        def decode_payload(payload_length, mask)
          pointer = 0

          # Read application data (unmasked if required)
          @frame.data.set_mask if mask
          pointer += 4 if mask
          payload = @frame.data.getbytes(pointer, payload_length)
          payload.force_encoding('UTF-8')
          pointer += payload_length
          @frame.data.unset_mask if mask

          # Throw away data up to pointer
          @frame.data.slice!(0...pointer)

          payload
        end

        def decode_continuation_frame(application_data, frame_type)
          @application_data_buffer ||= String.new('')
          @application_data_buffer << application_data
          @frame_type ||= frame_type
        end

        def decode_finish_continuation_frame(application_data)
          raise(WebSocket::Error::Frame::UnexpectedContinuationFrame) unless @frame_type
          @application_data_buffer << application_data
          # Test valid UTF-8 encoding
          raise(WebSocket::Error::Frame::InvalidPayloadEncoding) if @frame_type == :text && !@application_data_buffer.valid_encoding?
          message = @frame.class.new(version: @frame.version, type: @frame_type, data: @application_data_buffer, decoded: true)
          @application_data_buffer = nil
          @frame_type = nil
          message
        end
      end
    end
  end
end
