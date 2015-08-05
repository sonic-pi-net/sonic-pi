module Concurrent
  module Utility

    # @!visibility private
    module EngineDetector
      def on_jruby?
        ruby_engine == 'jruby'
      end

      def on_jruby_9000?
        on_jruby? && 0 == (JRUBY_VERSION =~ /^9\.0\.0\.0/)
      end

      def on_cruby?
        ruby_engine == 'ruby'
      end

      def on_rbx?
        ruby_engine == 'rbx'
      end

      def on_windows?
        !(RbConfig::CONFIG['host_os'] =~ /mswin|mingw|cygwin/).nil?
      end

      def ruby_engine
        defined?(RUBY_ENGINE) ? RUBY_ENGINE : 'ruby'
      end

      def ruby_version(comparison, major, minor = 0, patch = 0)
        result      = (RUBY_VERSION.split('.').map(&:to_i) <=> [major, minor, patch])
        comparisons = { :== => [0],
                        :>= => [1, 0],
                        :<= => [-1, 0],
                        :>  => [1],
                        :<  => [-1] }
        comparisons.fetch(comparison).include? result
      end
    end
  end

  # @!visibility private
  extend Utility::EngineDetector
end
