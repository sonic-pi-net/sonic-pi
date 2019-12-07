require 'stringio'
require 'minitest/unit'

class MiniTestResult

  minitest_version = Gem::Version.new(::MiniTest::Unit::VERSION)
  if Gem::Requirement.new('<= 4.6.1').satisfied_by?(minitest_version)
    FAILURE_PATTERN = %r{(Failure)\:\n([^\(]+)\(([^\)]+)\) \[([^\]]+)\]\:\n(.*)\n}m
    ERROR_PATTERN   = %r{(Error)\:\n([^\(]+)\(([^\)]+)\)\:\n(.+?)\n}m
    PATTERN_INDICES = { :method => 2, :testcase => 3 }
  else
    FAILURE_PATTERN = %r{(Failure)\:\n.([^#]+)\#([^ ]+) \[([^\]]+)\]\:\n(.*)\n}m
    ERROR_PATTERN   = %r{(Error)\:\n.([^#]+)\#([^ ]+)\:\n(.+?)\n}m
    PATTERN_INDICES = { :method => 3, :testcase => 2 }
  end

  def self.parse_failure(raw)
    matches = FAILURE_PATTERN.match(raw)
    return nil unless matches
    Failure.new(matches[PATTERN_INDICES[:method]], matches[PATTERN_INDICES[:testcase]], [matches[4]], matches[5])
  end

  def self.parse_error(raw)
    matches = ERROR_PATTERN.match(raw)
    return nil unless matches
    backtrace = raw.gsub(ERROR_PATTERN, '').split("\n").map(&:strip)
    Error.new(matches[PATTERN_INDICES[:method]], matches[PATTERN_INDICES[:testcase]], matches[4], backtrace)
  end

  class Failure
    attr_reader :method, :test_case, :location, :message
    def initialize(method, test_case, location, message)
      @method, @test_case, @location, @message = method, test_case, location, message
    end
  end

  class Error
    class Exception
      attr_reader :message, :backtrace
      def initialize(message, location)
        @message, @backtrace = message, location
      end
    end

    attr_reader :method, :test_case, :exception
    def initialize(method, test_case, message, backtrace)
      @method, @test_case, @exception = method, test_case, Exception.new(message, backtrace)
    end
  end

  def initialize(runner, tests)
    @runner, @tests = runner, tests
  end

  def failure_count
    @runner.failures
  end

  def assertion_count
    @tests.inject(0) { |total, test| total + test._assertions }
  end

  def error_count
    @runner.errors
  end

  def passed?
    @tests.all?(&:passed?)
  end

  def failures
    @runner.report.map { |puked| MiniTestResult.parse_failure(puked) }.compact
  end

  def errors
    @runner.report.map { |puked| MiniTestResult.parse_error(puked) }.compact
  end

  def failure_messages
    failures.map(&:message)
  end

  def failure_message_lines
    failure_messages.map { |message| message.split("\n") }.flatten
  end

  def error_messages
    errors.map { |e| e.exception.message }
  end
end
