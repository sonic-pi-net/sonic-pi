unless defined?(STANDARD_OBJECT_PUBLIC_INSTANCE_METHODS)
  STANDARD_OBJECT_PUBLIC_INSTANCE_METHODS = Object.instance_methods + Object.private_instance_methods
end

$:.unshift File.expand_path(File.join(File.dirname(__FILE__), "..", "lib"))
$:.unshift File.expand_path(File.join(File.dirname(__FILE__)))
$:.unshift File.expand_path(File.join(File.dirname(__FILE__), 'unit'))
$:.unshift File.expand_path(File.join(File.dirname(__FILE__), 'unit', 'parameter_matchers'))
$:.unshift File.expand_path(File.join(File.dirname(__FILE__), 'acceptance'))

require 'mocha/detection/mini_test'

begin
  require 'minitest'
rescue LoadError
end
begin
  require 'minitest/unit'
rescue LoadError
end

module Mocha; end

if (minitest_testcase = Mocha::Detection::MiniTest.testcase) && (ENV['MOCHA_RUN_INTEGRATION_TESTS'] != 'test-unit')
  begin
    require 'minitest/autorun'
  rescue LoadError
    MiniTest::Unit.autorun
  end
  class Mocha::TestCase < minitest_testcase
    def assert_nothing_raised(exception = StandardError)
      yield
    rescue exception => e
      flunk "Unexpected exception raised: #{e}"
    end

    alias_method :assert_not_nil, :refute_nil
    alias_method :assert_raise, :assert_raises
    alias_method :assert_not_same, :refute_same
    alias_method :assert_no_match, :refute_match
  end
else
  require 'test/unit'
  class Mocha::TestCase < Test::Unit::TestCase
    def test_dummy
      # Some versions (?) of Test::Unit try to run this base class as a test case
      # and it fails because it has no test methods, so I'm adding a dummy test.
    end
  end
end
