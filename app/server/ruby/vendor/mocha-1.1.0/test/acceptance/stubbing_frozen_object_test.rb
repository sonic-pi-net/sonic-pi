require File.expand_path('../acceptance_test_helper', __FILE__)
require 'mocha/setup'
require 'execution_point'

class StubbingFrozenObjectTest < Mocha::TestCase

  include AcceptanceTest

  def setup
    setup_acceptance_test
  end

  def teardown
    teardown_acceptance_test
  end

  def test_should_fail_fast_if_attempting_to_stub_method_on_frozen_object
    object = Object.new
    object.freeze
    execution_point = nil
    test_result = run_as_test do
      execution_point = ExecutionPoint.current; object.stubs(:stubbed_method)
    end
    assert_failed(test_result)
    assert_equal 1, test_result.error_count
    assert_equal execution_point, ExecutionPoint.new(test_result.errors[0].exception.backtrace)
  end

  def test_should_fail_fast_if_attempting_to_expect_method_on_frozen_object
    object = Object.new
    object.freeze
    execution_point = nil
    test_result = run_as_test do
      execution_point = ExecutionPoint.current; object.expects(:stubbed_method)
    end
    assert_failed(test_result)
    assert_equal 1, test_result.error_count
    assert_equal execution_point, ExecutionPoint.new(test_result.errors[0].exception.backtrace)
  end

  def test_should_fail_fast_if_attempting_to_stub_method_on_frozen_class
    klass = Class.new
    klass.freeze
    execution_point = nil
    test_result = run_as_test do
      execution_point = ExecutionPoint.current; klass.stubs(:stubbed_method)
    end
    assert_failed(test_result)
    assert_equal 1, test_result.error_count
    assert_equal execution_point, ExecutionPoint.new(test_result.errors[0].exception.backtrace)
  end

  def test_should_fail_fast_if_attempting_to_expect_method_on_frozen_class
    klass = Class.new
    klass.freeze
    execution_point = nil
    test_result = run_as_test do
      execution_point = ExecutionPoint.current; klass.expects(:stubbed_method)
    end
    assert_failed(test_result)
    assert_equal 1, test_result.error_count
    assert_equal execution_point, ExecutionPoint.new(test_result.errors[0].exception.backtrace)
  end

  def test_should_fail_fast_if_attempting_to_stub_method_on_any_instance_of_frozen_class
    klass = Class.new
    klass.freeze
    execution_point = nil
    test_result = run_as_test do
      execution_point = ExecutionPoint.current; klass.any_instance.stubs(:stubbed_method)
    end
    assert_failed(test_result)
    assert_equal 1, test_result.error_count
    assert_equal execution_point, ExecutionPoint.new(test_result.errors[0].exception.backtrace)
  end

  def test_should_fail_fast_if_attempting_to_expect_method_on_any_instance_of_frozen_class
    klass = Class.new
    klass.freeze
    execution_point = nil
    test_result = run_as_test do
      execution_point = ExecutionPoint.current; klass.any_instance.expects(:stubbed_method)
    end
    assert_failed(test_result)
    assert_equal 1, test_result.error_count
    assert_equal execution_point, ExecutionPoint.new(test_result.errors[0].exception.backtrace)
  end
end
