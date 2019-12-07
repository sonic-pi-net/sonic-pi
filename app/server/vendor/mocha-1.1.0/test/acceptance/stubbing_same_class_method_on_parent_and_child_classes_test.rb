require File.expand_path('../acceptance_test_helper', __FILE__)
require 'mocha/setup'

class StubbingSameClassMethodOnParentAndChildClassTest < Mocha::TestCase

  include AcceptanceTest

  def setup
    setup_acceptance_test
  end

  def teardown
    teardown_acceptance_test
  end

  def test_stubbing_same_method_on_parent_and_child_classes
    parent_class = Class.new do
      def self.foo
        "Parent.foo"
      end
    end
    child_class = Class.new(parent_class)
    test_result = run_as_tests(
      :test_1 => lambda {
        parent_class.stubs(:foo).returns("stubbed Parent.foo")
        child_class.stubs(:foo).returns("stubbed Child.foo")
      },
      :test_2 => lambda {
        parent_class.foo
        child_class.foo
      }
    )
    assert_passed(test_result)
  end
end
