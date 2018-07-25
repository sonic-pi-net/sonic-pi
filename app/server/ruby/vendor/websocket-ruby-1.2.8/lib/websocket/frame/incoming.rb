# frozen_string_literal: true

module WebSocket
  module Frame
    # Construct or parse incoming WebSocket Frame.
    # @note You should NEVER use this class directly - use Client or Server subclasses instead, as they contain additional frame options(i.e. Client-side masking in draft 04)
    #
    # @example
    #   frame = WebSocket::Frame::Incoming::Server.new(version: @handshake.version)
    #   frame << "\x81\x05\x48\x65\x6c\x6c\x6f\x81\x06\x77\x6f\x72\x6c\x64\x21"
    #   frame.next # "Hello"
    #   frame.next # "world!""
    class Incoming < Base
      autoload :Client, "#{::WebSocket::ROOT}/websocket/frame/incoming/client"
      autoload :Server, "#{::WebSocket::ROOT}/websocket/frame/incoming/server"

      def initialize(args = {})
        @decoded = args[:decoded] || false
        super
      end

      # If data is still encoded after receiving then this is false. After calling "next" you will receive
      # another instance of incoming frame, but with data decoded - this function will return true and
      # to_s will return frame content instead of raw data.
      # @return [Boolean] If frame already decoded?
      def decoded?
        @decoded
      end

      # Add provided string as raw incoming frame.
      # @param data [String] Raw frame
      def <<(data)
        @data << data
      end

      # Return next complete frame.
      # This function will merge together splitted frames and return as combined content.
      # Check #error if nil received to check for eventual parsing errors
      # @return [WebSocket::Frame::Incoming] Single incoming frame or nil if no complete frame is available.
      def next
        @handler.decode_frame unless decoded?
      end
      rescue_method :next

      # If decoded then this will return frame content. Otherwise it will return raw frame.
      # @return [String] Data of frame
      def to_s
        @data
      end
    end
  end
end
