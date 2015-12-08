require File.expand_path('../acceptance_test_helper', __FILE__)
require 'mocha/setup'

class UnstubbingTest < Mocha::TestCase

  include AcceptanceTest

  def setup
    setup_acceptance_test
  end

  def teardown
    teardown_acceptance_test
  end

  def test_unstubbing_an_instance_method_should_restore_original_behaviour
    klass = Class.new do
      def my_instance_method; :original_return_value; end
    end
    test_result = run_as_test do
      object = klass.new
      object.stubs(:my_instance_method).returns(:new_return_value)
      object.unstub(:my_instance_method)
      assert_equal :original_return_value, object.my_instance_method
    end
    assert_passed(test_result)
  end

  def test_unstubbing_a_class_method_should_restore_original_behaviour
    klass = Class.new do
      def self.my_class_method; :original_return_value; end
    end
    test_result = run_as_test do
      klass.stubs(:my_class_method).returns(:new_return_value)
      klass.unstub(:my_class_method)
      assert_equal :original_return_value, klass.my_class_method
    end
    assert_passed(test_result)
  end

  def test_unstubbing_a_module_method_should_restore_original_behaviour
    mod = Module.new do
      def self.my_module_method; :original_return_value; end
    end
    test_result = run_as_test do
      mod.stubs(:my_module_method).returns(:new_return_value)
      mod.unstub(:my_module_method)
      assert_equal :original_return_value, mod.my_module_method
    end
    assert_passed(test_result)
  end

  def test_unstubbing_a_module_method_defined_like_fileutils_in_ruby_2_0_should_restore_original_behaviour
    mod = Module.new do
      def my_module_method; :original_return_value; end
      private :my_module_method
      extend self
      class << self
        public :my_module_method
      end
    end
    test_result = run_as_test do
      mod.stubs(:my_module_method).returns(:new_return_value)
      mod.unstub(:my_module_method)
      assert_equal :original_return_value, mod.my_module_method
    end
    assert_passed(test_result)
  end

  def test_unstubbing_an_any_instance_method_should_restore_original_behaviour
    klass = Class.new do
      def my_instance_method; :original_return_value; end
    end
    test_result = run_as_test do
      object = klass.new
      klass.any_instance.stubs(:my_instance_method).returns(:new_return_value)
      klass.any_instance.unstub(:my_instance_method)
      assert_equal :original_return_value, object.my_instance_method
    end
    assert_passed(test_result)
  end

  def test_unstubbing_multiple_methods_should_restore_original_behaviour
    klass = Class.new do
      def my_first_instance_method; :original_return_value; end
      def my_second_instance_method; :original_return_value; end
    end
    test_result = run_as_test do
      object = klass.new
      object.stubs(:my_first_instance_method).returns(:new_return_value)
      object.stubs(:my_second_instance_method).returns(:new_return_value)
      object.unstub(:my_first_instance_method, :my_second_instance_method)
      assert_equal :original_return_value, object.my_first_instance_method
      assert_equal :original_return_value, object.my_second_instance_method
    end
    assert_passed(test_result)
  end

  def test_unstubbing_a_method_multiple_times_should_restore_original_behaviour
    klass = Class.new do
      def my_instance_method; :original_return_value; end
    end
    test_result = run_as_test do
      object = klass.new
      object.stubs(:my_instance_method).returns(:new_return_value)
      object.unstub(:my_instance_method)
      object.unstub(:my_instance_method)
      assert_equal :original_return_value, object.my_instance_method
    end
    assert_passed(test_result)
  end

  def test_unstubbing_a_non_stubbed_method_should_do_nothing
    klass = Class.new do
      def my_instance_method; :original_return_value; end
    end
    test_result = run_as_test do
      object = klass.new
      object.unstub(:my_instance_method)
      assert_equal :original_return_value, object.my_instance_method
    end
    assert_passed(test_result)
  end

  def test_unstubbing_a_method_which_was_stubbed_multiple_times_should_restore_orginal_behaviour
    klass = Class.new do
      def my_instance_method; :original_return_value; end
    end
    test_result = run_as_test do
      object = klass.new
      object.stubs(:my_instance_method).with(:first).returns(:first_new_return_value)
      object.stubs(:my_instance_method).with(:second).returns(:second_new_return_value)
      object.unstub(:my_instance_method)
      assert_equal :original_return_value, object.my_instance_method
    end
    assert_passed(test_result)
  end

  def test_unstubbing_a_method_should_not_unstub_other_stubbed_methods
    klass = Class.new do
      def my_first_instance_method; :first_return_value; end
      def my_second_instance_method; :second_return_value; end
    end

    test_result = run_as_test do
      object = klass.new
      object.stubs(:my_first_instance_method).returns(:first_new_return_value)
      object.stubs(:my_second_instance_method).returns(:second_new_return_value)
      object.unstub(:my_first_instance_method)
      assert_equal :first_return_value, object.my_first_instance_method
      assert_equal :second_new_return_value, object.my_second_instance_method
    end
    assert_passed(test_result)
  end

  def test_unstubbing_a_method_should_remove_all_expectations_for_that_method
    klass = Class.new do
      def my_instance_method; :original_return_value; end
    end
    test_result = run_as_test do
      object = klass.new
      object.expects(:my_instance_method).with(:first)
      object.expects(:my_instance_method).with(:second)
      object.unstub(:my_instance_method)
    end
    assert_passed(test_result)
  end
end
