module WebSocket
  class Error < RuntimeError

    class Frame < ::WebSocket::Error

      class ControlFramePayloadTooLong < ::WebSocket::Error::Frame
        def message; :control_frame_payload_too_long; end
      end

      class DataFrameInsteadContinuation < ::WebSocket::Error::Frame
        def message; :data_frame_instead_continuation; end
      end

      class FragmentedControlFrame < ::WebSocket::Error::Frame
        def message; :fragmented_control_frame; end
      end

      class Invalid < ::WebSocket::Error::Frame
        def message; :invalid_frame; end
      end

      class InvalidPayloadEncoding < ::WebSocket::Error::Frame
        def message; :invalid_payload_encoding; end
      end

      class MaskTooShort < ::WebSocket::Error::Frame
        def message; :mask_is_too_short; end
      end

      class ReservedBitUsed < ::WebSocket::Error::Frame
        def message; :reserved_bit_used; end
      end

      class TooLong < ::WebSocket::Error::Frame
        def message; :frame_too_long; end
      end

      class UnexpectedContinuationFrame < ::WebSocket::Error::Frame
        def message; :unexpected_continuation_frame; end
      end

      class UnknownFrameType < ::WebSocket::Error::Frame
        def message; :unknown_frame_type; end
      end

      class UnknownOpcode < ::WebSocket::Error::Frame
        def message; :unknown_opcode; end
      end

      class UnknownVersion < ::WebSocket::Error::Frame
        def message; :unknown_protocol_version; end
      end

    end

    class Handshake < ::WebSocket::Error

      class GetRequestRequired < ::WebSocket::Error::Handshake
        def message; :get_request_required; end
      end

      class InvalidAuthentication < ::WebSocket::Error::Handshake
        def message; :invalid_handshake_authentication; end
      end

      class InvalidHeader < ::WebSocket::Error::Handshake
        def message; :invalid_header; end
      end

      class InvalidStatusCode < ::WebSocket::Error::Handshake
        def message; :invalid_status_code; end
      end

      class NoHostProvided < ::WebSocket::Error::Handshake
        def message; :no_host_provided; end
      end

      class UnknownVersion < ::WebSocket::Error::Handshake
        def message; :unknown_protocol_version; end
      end

    end

  end
end
