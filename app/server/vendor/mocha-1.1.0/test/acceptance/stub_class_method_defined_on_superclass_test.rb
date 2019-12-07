require File.expand_path('../acceptance_test_helper', __FILE__)
require 'mocha/setup'

class StubClassMethodDefinedOnSuperclassTest < Mocha::TestCase

  include AcceptanceTest

  def setup
    setup_acceptance_test
  end

  def teardown
    teardown_acceptance_test
  end

  def test_should_stub_public_method_on_child_class_and_leave_it_unchanged_after_test
    superklass = Class.new do
      class << self
        def my_class_method
          :original_return_value
        end
        public :my_class_method
      end
    end
    klass = Class.new(superklass)
    assert_snapshot_unchanged(klass) do
      test_result = run_as_test do
        klass.stubs(:my_class_method).returns(:new_return_value)
        assert_equal :new_return_value, klass.my_class_method
      end
      assert_passed(test_result)
    end
    assert_equal :original_return_value, klass.my_class_method
  end

  def test_should_stub_protected_method_on_child_class_and_leave_it_unchanged_after_test
    superklass = Class.new do
      class << self
        def my_class_method
          :original_return_value
        end
        protected :my_class_method
      end
    end
    klass = Class.new(superklass)
    assert_snapshot_unchanged(klass) do
      test_result = run_as_test do
        klass.stubs(:my_class_method).returns(:new_return_value)
        assert_equal :new_return_value, klass.send(:my_class_method)
      end
      assert_passed(test_result)
    end
    assert_equal :original_return_value, klass.send(:my_class_method)
  end

  def test_should_stub_private_method_on_child_class_and_leave_it_unchanged_after_test
    superklass = Class.new do
      class << self
        def my_class_method
          :original_return_value
        end
        private :my_class_method
      end
    end
    klass = Class.new(superklass)
    assert_snapshot_unchanged(klass) do
      test_result = run_as_test do
        klass.stubs(:my_class_method).returns(:new_return_value)
        assert_equal :new_return_value, klass.send(:my_class_method)
      end
      assert_passed(test_result)
    end
    assert_equal :original_return_value, klass.send(:my_class_method)
  end

  def test_should_stub_method_on_superclass_and_leave_it_unchanged_after_test
    superklass = Class.new do
      class << self
        def my_class_method
          :original_return_value
        end
        public :my_class_method
      end
    end
    klass = Class.new(superklass)
    assert_snapshot_unchanged(klass) do
      test_result = run_as_test do
        superklass.stubs(:my_class_method).returns(:new_return_value)
        assert_equal :new_return_value, klass.my_class_method
      end
      assert_passed(test_result)
    end
    assert_equal :original_return_value, klass.my_class_method
  end

  def test_stub_on_earliest_receiver_should_take_priority
    superklass = Class.new do
      class << self
        def my_class_method
          :original_return_value
        end
      end
    end
    klass = Class.new(superklass)
    test_result = run_as_test do
      klass.stubs(:my_class_method).returns(:klass_return_value)
      superklass.stubs(:my_class_method).returns(:superklass_return_value)
      assert_equal :klass_return_value, klass.my_class_method
    end
    assert_passed(test_result)
  end
end
