# encoding: binary
# frozen_string_literal: true

module WebSocket
  module Frame
    module Handler
      class Handler75 < Base
        # @see WebSocket::Frame::Base#supported_frames
        def supported_frames
          %i[text close]
        end

        # @see WebSocket::Frame::Handler::Base#encode_frame
        def encode_frame
          case @frame.type
          when :close then "\xff\x00"
          when :text then
            ary = ["\x00", @frame.data, "\xff"]
            ary.map { |s| s.encode('UTF-8', 'UTF-8', invalid: :replace) }
            ary.join
          else raise WebSocket::Error::Frame::UnknownFrameType
          end
        end

        # @see WebSocket::Frame::Handler::Base#decode_frame
        def decode_frame
          return if @frame.data.size.zero?

          pointer = 0
          frame_type = @frame.data.getbyte(pointer)
          pointer += 1

          if (frame_type & 0x80) == 0x80
            # If the high-order bit of the /frame type/ byte is set
            length = 0

            loop do
              return unless @frame.data.getbyte(pointer)
              b = @frame.data.getbyte(pointer)
              pointer += 1
              b_v = b & 0x7F
              length = length * 128 + b_v
              break unless (b & 0x80) == 0x80
            end

            raise WebSocket::Error::Frame::TooLong if length > ::WebSocket.max_frame_size

            unless @frame.data.getbyte(pointer + length - 1).nil?
              # Straight from spec - I'm sure this isn't crazy...
              # 6. Read /length/ bytes.
              # 7. Discard the read bytes.
              @frame.instance_variable_set '@data', @frame.data[(pointer + length)..-1]

              # If the /frame type/ is 0xFF and the /length/ was 0, then close
              if length.zero?
                @frame.class.new(version: @frame.version, type: :close, decoded: true)
              end
            end
          else
            # If the high-order bit of the /frame type/ byte is _not_ set

            raise WebSocket::Error::Frame::Invalid if @frame.data.getbyte(0) != 0x00

            # Addition to the spec to protect against malicious requests
            raise WebSocket::Error::Frame::TooLong if @frame.data.size > ::WebSocket.max_frame_size

            msg = @frame.data.slice!(/\A\x00[^\xff]*\xff/)
            if msg
              msg.gsub!(/\A\x00|\xff\z/, '')
              msg.force_encoding('UTF-8')
              @frame.class.new(version: @frame.version, type: :text, data: msg, decoded: true)
            end
          end
        end
      end
    end
  end
end
