# frozen_string_literal: true

module WebSocket
  module Frame
    class Base
      def incoming_masking?
        @handler.masking?
      end

      def outgoing_masking?
        false
      end
    end
  end
end
