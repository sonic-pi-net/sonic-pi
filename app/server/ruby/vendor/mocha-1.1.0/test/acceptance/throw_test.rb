require File.expand_path('../acceptance_test_helper', __FILE__)
require 'mocha/setup'

class ThrowTest < Mocha::TestCase

  include AcceptanceTest

  def setup
    setup_acceptance_test
  end

  def teardown
    teardown_acceptance_test
  end

  def test_should_throw_tag
    test_result = run_as_test do
      foo = stub('foo')
      foo.stubs(:bar).throws(:tag)
      assert_throws(:tag) { foo.bar }
    end
    assert_passed(test_result)
  end

  def test_should_throw_with_return_value
    test_result = run_as_test do
      foo = stub('foo')
      foo.stubs(:bar).throws(:tag, 'return-value')
      return_value = catch(:tag) { foo.bar }
      assert_equal 'return-value', return_value
    end
    assert_passed(test_result)
  end

  def test_should_throw_two_different_tags
    test_result = run_as_test do
      foo = stub('foo')
      foo.stubs(:bar).throws(:tag_one).then.throws(:tag_two)
      assert_throws(:tag_one) { foo.bar }
      assert_throws(:tag_two) { foo.bar }
    end
    assert_passed(test_result)
  end

end
