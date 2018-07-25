# frozen_string_literal: true

module WebSocket
  module Frame
    class Outgoing
      class Client < Outgoing
        def incoming_masking?
          false
        end

        def outgoing_masking?
          @handler.masking?
        end
      end
    end
  end
end
