# frozen_string_literal: true

module WebSocket
  module Frame
    # Construct or parse incoming WebSocket Frame.
    # @note You should NEVER use this class directly - use Client or Server subclasses instead, as they contain additional frame options(i.e. Client-side masking in draft 04)
    #
    # @example
    #   frame = WebSocket::Frame::Outgoing::Server.new(version: @handshake.version, data: "Hello", type: :text)
    #   frame.to_s # "\x81\x05\x48\x65\x6c\x6c\x6f"
    class Outgoing < Base
      autoload :Client, "#{::WebSocket::ROOT}/websocket/frame/outgoing/client"
      autoload :Server, "#{::WebSocket::ROOT}/websocket/frame/outgoing/server"

      # Is selected type supported by current draft version?
      # @return [Boolean] true if frame type is supported
      def supported?
        support_type?
      end

      # Should current frame be sent? Exclude empty frames etc.
      # @return [Boolean] true if frame should be sent
      def require_sending?
        !error?
      end

      # Return raw frame formatted for sending.
      def to_s
        raise WebSocket::Error::Frame::UnknownFrameType unless supported?
        @handler.encode_frame
      end
      rescue_method :to_s
    end
  end
end
