require File.expand_path('../acceptance_test_helper', __FILE__)
require 'mocha/setup'

class Issue70Test < Mocha::TestCase

  include AcceptanceTest

  def setup
    setup_acceptance_test
  end

  def teardown
    teardown_acceptance_test
  end

  def test_should_stub_expectations_instance_method
    instance = Class.new do
      def expectations
        :original_return_value
      end
    end.new
    test_result = run_as_test do
      instance.stubs(:expectations).returns(:stubbed_return_value)
      assert_equal :stubbed_return_value, instance.expectations
    end
    assert_passed(test_result)
  end

  def test_should_stub_expectations_class_method
    klass = Class.new do
      def self.expectations
        :original_return_value
      end
    end
    test_result = run_as_test do
      klass.stubs(:expectations).returns(:stubbed_return_value)
      assert_equal :stubbed_return_value, klass.expectations
    end
    assert_passed(test_result)
  end

  def test_should_stub_expectations_any_instance_method
    klass = Class.new do
      def expectations
        :original_return_value
      end
    end
    instance = klass.new
    test_result = run_as_test do
      klass.any_instance.stubs(:expectations).returns(:stubbed_return_value)
      assert_equal :stubbed_return_value, instance.expectations
    end
    assert_passed(test_result)
  end
end
