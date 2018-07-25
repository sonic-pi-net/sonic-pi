# frozen_string_literal: true

module WebSocket
  module ExceptionHandler
    attr_accessor :error

    def self.included(base)
      base.extend(ClassMethods)
    end

    module ClassMethods
      # Rescue from WebSocket::Error errors.
      #
      # @param [String] method_name Name of method that should be wrapped and rescued
      # @param [Hash] options Options for rescue
      #
      # @options options [Any] :return Value that should be returned instead of raised error
      def rescue_method(method_name, options = {})
        define_method "#{method_name}_with_rescue" do |*args|
          begin
            send("#{method_name}_without_rescue", *args)
          rescue WebSocket::Error => e
            self.error = e.message.to_sym
            WebSocket.should_raise ? raise : options[:return]
          end
        end
        alias_method "#{method_name}_without_rescue", method_name
        alias_method method_name, "#{method_name}_with_rescue"
      end
    end
  end
end
