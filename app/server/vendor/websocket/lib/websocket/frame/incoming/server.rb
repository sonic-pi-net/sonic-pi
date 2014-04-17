module WebSocket
  module Frame
    class Incoming
      class Server < Incoming

        def incoming_masking?
          @handler.masking?
        end

        def outgoing_masking?
          false
        end

      end
    end
  end
end
