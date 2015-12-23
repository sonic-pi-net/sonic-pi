require 'test/unit/testresult'

class TestUnitResult
  def self.build_test_result
    test_result = Test::Unit::TestResult.new
    class << test_result
      attr_reader :failures, :errors
      def failure_messages
        failures.map { |failure| failure.message }
      end
      def failure_message_lines
        failure_messages.map { |message| message.split("\n") }.flatten
      end
      def error_messages
        errors.map { |error| error.message }
      end
    end
    test_result
  end
end
