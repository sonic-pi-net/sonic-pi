module WebSocket
  module ExceptionHandler

    attr_reader :error

    def self.included(base)
      base.extend(ClassMethods)
    end

    private

    # Changes state to error and sets error message
    # @param [String] message Error message to set
    def set_error(message)
      @error = message
    end

    module ClassMethods

      def rescue_method(method_name, options = {})
        define_method "#{method_name}_with_rescue" do |*args|
          begin
            send("#{method_name}_without_rescue", *args)
          rescue WebSocket::Error => e
            set_error(e.message.to_sym)
            WebSocket.should_raise ? raise : options[:return]
          end
        end
        alias_method "#{method_name}_without_rescue", method_name
        alias_method method_name, "#{method_name}_with_rescue"
      end

    end

  end
end
