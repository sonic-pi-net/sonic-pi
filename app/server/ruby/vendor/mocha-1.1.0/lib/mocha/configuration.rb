module Mocha

  # Configuration settings.
  class Configuration

    DEFAULTS = {
      :stubbing_method_unnecessarily => :allow,
      :stubbing_method_on_non_mock_object => :allow,
      :stubbing_non_existent_method => :allow,
      :stubbing_non_public_method => :allow,
      :stubbing_method_on_nil => :prevent,
    }

    class << self

      # Allow the specified +action+.
      #
      # @param [Symbol] action one of +:stubbing_method_unnecessarily+, +:stubbing_method_on_non_mock_object+, +:stubbing_non_existent_method+, +:stubbing_non_public_method+, +:stubbing_method_on_nil+.
      # @yield optional block during which the configuration change will be changed before being returned to its original value at the end of the block.
      def allow(action, &block)
        change_config action, :allow, &block
      end

      # @private
      def allow?(action)
        configuration[action] == :allow
      end

      # Warn if the specified +action+ is attempted.
      #
      # @param [Symbol] action one of +:stubbing_method_unnecessarily+, +:stubbing_method_on_non_mock_object+, +:stubbing_non_existent_method+, +:stubbing_non_public_method+, +:stubbing_method_on_nil+.
      # @yield optional block during which the configuration change will be changed before being returned to its original value at the end of the block.
      def warn_when(action, &block)
        change_config action, :warn, &block
      end

      # @private
      def warn_when?(action)
        configuration[action] == :warn
      end

      # Raise a {StubbingError} if if the specified +action+ is attempted.
      #
      # @param [Symbol] action one of +:stubbing_method_unnecessarily+, +:stubbing_method_on_non_mock_object+, +:stubbing_non_existent_method+, +:stubbing_non_public_method+, +:stubbing_method_on_nil+.
      # @yield optional block during which the configuration change will be changed before being returned to its original value at the end of the block.
      def prevent(action, &block)
        change_config action, :prevent, &block
      end

      # @private
      def prevent?(action)
        configuration[action] == :prevent
      end

      # @private
      def reset_configuration
        @configuration = nil
      end

      private

      # @private
      def configuration
        @configuration ||= DEFAULTS.dup
      end

      # @private
      def change_config(action, new_value, &block)
        if block_given?
          temporarily_change_config action, new_value, &block
        else
          configuration[action] = new_value
        end
      end

      # @private
      def temporarily_change_config(action, new_value, &block)
        original_value = configuration[action]
        configuration[action] = new_value
        yield
      ensure
        configuration[action] = original_value
      end

    end

  end

end
