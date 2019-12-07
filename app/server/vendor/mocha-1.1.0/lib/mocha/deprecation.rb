require 'mocha/debug'

module Mocha

  class Deprecation

    class << self

      attr_accessor :mode, :messages

      def warning(message)
        @messages << message
        $stderr.puts "\n*** Mocha deprecation warning: #{message}\n\n" unless mode == :disabled
        $stderr.puts caller.join("\n  ") if mode == :debug
      end

    end

    self.mode = Debug::OPTIONS['debug'] ? :debug : :enabled
    self.messages = []

  end

end
