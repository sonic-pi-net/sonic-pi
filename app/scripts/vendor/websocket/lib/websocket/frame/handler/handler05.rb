# encoding: binary

module WebSocket
  module Frame
    module Handler
      class Handler05 < Handler04

        def encode_frame
          if @frame.code
            @frame.data = Data.new([@frame.code].pack('n') + @frame.data.to_s)
            @frame.code = nil
          end
          super
        end

        # Since handler 5 masking should be enabled by default
        def masking?; true; end

      end
    end
  end
end
