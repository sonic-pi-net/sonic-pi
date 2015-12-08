require File.expand_path('../acceptance_test_helper', __FILE__)
require 'mocha/setup'

class StubAnyInstanceMethodDefinedOnSuperclassTest < Mocha::TestCase

  include AcceptanceTest

  def setup
    setup_acceptance_test
  end

  def teardown
    teardown_acceptance_test
  end

  def test_should_stub_method_and_leave_it_unchanged_after_test
    superklass = Class.new do
      def my_superclass_method
        :original_return_value
      end
      public :my_superclass_method
    end
    klass = Class.new(superklass)
    instance = klass.new
    assert_snapshot_unchanged(instance) do
      test_result = run_as_test do
        superklass.any_instance.stubs(:my_superclass_method).returns(:new_return_value)
        assert_equal :new_return_value, instance.my_superclass_method
      end
      assert_passed(test_result)
    end
    assert_equal :original_return_value, instance.my_superclass_method
  end
end