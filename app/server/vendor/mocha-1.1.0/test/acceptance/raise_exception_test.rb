require File.expand_path('../acceptance_test_helper', __FILE__)
require 'mocha/setup'

class RaiseExceptionTest < Mocha::TestCase

  include AcceptanceTest

  def setup
    setup_acceptance_test
  end

  def teardown
    teardown_acceptance_test
  end

  def test_should_raise_exception
    exception_class = Class.new(StandardError)
    test_result = run_as_test do
      foo = stub('foo')
      foo.stubs(:bar).raises(exception_class, "my-message")
      exception = assert_raises(exception_class) { foo.bar }
      assert_equal "my-message", exception.message
    end
    assert_passed(test_result)
  end

  def test_should_raise_two_different_exceptions
    exception_one_class = Class.new(StandardError)
    exception_two_class = Class.new(StandardError)
    test_result = run_as_test do
      foo = stub('foo')
      foo.stubs(:bar).raises(exception_one_class).then.raises(exception_two_class)
      assert_raises(exception_one_class) { foo.bar }
      assert_raises(exception_two_class) { foo.bar }
    end
    assert_passed(test_result)
  end

end
