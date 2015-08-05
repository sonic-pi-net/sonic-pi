require 'concurrent/synchronization/abstract_object' # for JRuby
require 'concurrent/utility/engine'

module Concurrent
  module Utility

    # @!visibility private
    module NativeExtensionLoader

      @c_ext_loaded    ||= false
      @java_ext_loaded ||= false

      # @!visibility private
      def allow_c_extensions?
        Concurrent.on_cruby?
      end

      if Concurrent.on_cruby? && !@c_ext_loaded
        tries = [
          lambda do
            require 'concurrent/extension'
            @c_ext_loaded = true
          end,
          lambda do
            # may be a Windows cross-compiled native gem
            require "concurrent/#{RUBY_VERSION[0..2]}/extension"
            @c_ext_loaded = true
          end,
          lambda do
            warn 'Performance on MRI may be improved with the concurrent-ruby-ext gem. Please see http://concurrent-ruby.com'
          end]

        tries.each do |try|
          begin
            try.call
            break
          rescue LoadError
            next
          end
        end
      end

      if Concurrent.on_jruby? && !@java_ext_loaded
        begin
          require 'concurrent_ruby_ext'
          @java_ext_loaded = true
        rescue LoadError
          warn 'Performance on JRuby may be improved by installing the pre-compiled Java extensions. Please see http://concurrent-ruby.com'
        end
      end
    end
  end

  # @!visibility private
  extend Utility::NativeExtensionLoader
end
