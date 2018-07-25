# encoding: binary
# frozen_string_literal: true

module WebSocket
  module Frame
    module Handler
      class Handler04 < Handler03
        private

        # The only difference between draft 03 framing and draft 04 framing is
        # that the MORE bit has been changed to a FIN bit
        def fin
          true
        end
      end
    end
  end
end
