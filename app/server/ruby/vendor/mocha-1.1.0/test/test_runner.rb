require 'assertions'

require 'mocha/detection/mini_test'

module TestRunner
  def run_as_test(&block)
    run_as_tests(:test_me => block)
  end

  def run_as_tests(methods = {})
    base_class = Mocha::TestCase
    test_class = Class.new(base_class) do
      include Assertions

      methods.each do |(method_name, proc)|
        define_method(method_name, proc)
      end
    end

    tests = methods.keys.select { |m| m.to_s[/^test/] }.map { |m| test_class.new(m) }

    if Mocha::Detection::MiniTest.testcase && (ENV['MOCHA_RUN_INTEGRATION_TESTS'] != 'test-unit')
      minitest_version = Gem::Version.new(Mocha::Detection::MiniTest.version)
      if Gem::Requirement.new('>= 5.0.0').satisfied_by?(minitest_version)
        require File.expand_path('../minitest_result', __FILE__)
        tests.each do |test|
          test.run
        end
        Minitest::Runnable.runnables.delete(test_class)
        test_result = MinitestResult.new(tests)
      elsif Gem::Requirement.new('> 0.0.0', '< 5.0.0').satisfied_by?(minitest_version)
        require File.expand_path('../mini_test_result', __FILE__)
        runner = MiniTest::Unit.new
        tests.each do |test|
          test.run(runner)
        end
        test_result = MiniTestResult.new(runner, tests)
      end
    else
      require File.expand_path('../test_unit_result', __FILE__)
      test_result = TestUnitResult.build_test_result
      tests.each do |test|
        test.run(test_result) {}
      end
    end

    test_result
  end

  def assert_passed(test_result)
    flunk "Test failed unexpectedly with message: #{test_result.failures}" if test_result.failure_count > 0
    flunk "Test failed unexpectedly with message: #{test_result.errors}" if test_result.error_count > 0
  end

  def assert_failed(test_result)
    flunk "Test passed unexpectedly" unless test_result.failure_count + test_result.error_count > 0
  end
end
