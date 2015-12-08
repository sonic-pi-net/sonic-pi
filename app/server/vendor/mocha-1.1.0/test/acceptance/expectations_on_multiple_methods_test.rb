require File.expand_path('../acceptance_test_helper', __FILE__)
require 'mocha/setup'

class ExpectationsOnMultipleMethodsTest < Mocha::TestCase

  include AcceptanceTest

  def setup
    setup_acceptance_test
  end

  def teardown
    teardown_acceptance_test
  end

  def test_should_expect_calls_to_multiple_methods
    instance = Class.new do
      def my_instance_method_1
        :original_return_value_1
      end
      def my_instance_method_2
        :original_return_value_2
      end
    end.new
    test_result = run_as_test do
      instance.expects(
        :my_instance_method_1 => :new_return_value_1,
        :my_instance_method_2 => :new_return_value_2
      )
      assert_equal :new_return_value_1, instance.my_instance_method_1
      assert_equal :new_return_value_2, instance.my_instance_method_2
    end
    assert_passed(test_result)
  end

  def test_should_stub_calls_to_multiple_methods
    instance = Class.new do
      def my_instance_method_1
        :original_return_value_1
      end
      def my_instance_method_2
        :original_return_value_2
      end
    end.new
    test_result = run_as_test do
      instance.stubs(
        :my_instance_method_1 => :new_return_value_1,
        :my_instance_method_2 => :new_return_value_2
      )
      assert_equal :new_return_value_1, instance.my_instance_method_1
      assert_equal :new_return_value_2, instance.my_instance_method_2
    end
    assert_passed(test_result)
  end
end
