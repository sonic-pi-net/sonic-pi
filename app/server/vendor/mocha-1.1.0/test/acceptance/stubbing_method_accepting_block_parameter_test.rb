require File.expand_path('../acceptance_test_helper', __FILE__)
require 'mocha/setup'

class StubbingMethodAcceptingBlockParameterTest < Mocha::TestCase

  include AcceptanceTest

  def setup
    setup_acceptance_test
  end

  def teardown
    teardown_acceptance_test
  end

  def test_stubbing_class_method_accepting_block_parameter_should_restore_original_method
    klass = Class.new do
      def self.my_class_method(&block); block.call; end
    end
    test_result = run_as_test do
      klass.stubs(:my_class_method)
    end
    assert_passed(test_result)
    assert_equal :return_value, klass.my_class_method { :return_value }
  end

  def test_stubbing_instance_method_accepting_block_parameter_should_restore_original_method
    instance = Class.new do
      def my_instance_method(&block); block.call; end
    end.new
    test_result = run_as_test do
      instance.stubs(:my_instance_method)
    end
    assert_passed(test_result)
    assert_equal :return_value, instance.my_instance_method { :return_value }
  end

  def test_stubbing_any_instance_method_accepting_block_parameter_should_restore_original_method
    klass = Class.new do
      def my_instance_method(&block); block.call; end
    end
    test_result = run_as_test do
      klass.any_instance.stubs(:my_instance_method)
    end
    assert_passed(test_result)
    assert_equal :return_value, klass.new.my_instance_method { :return_value }
  end
end
