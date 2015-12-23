require File.expand_path('../acceptance_test_helper', __FILE__)
require 'mocha/setup'

class UnexpectedInvocationTest < Mocha::TestCase

  include AcceptanceTest

  def setup
    setup_acceptance_test
  end

  def teardown
    teardown_acceptance_test
  end

  def test_avoid_recursion_when_unexpected_invocation_exception_message_depends_on_uninspectable_object
    test_result = run_as_test do
      instance = Class.new.new
      instance.expects(:inspect).never
      instance.inspect(1, 2, 'foo')
    end
    assert_failed(test_result)
    assert_equal "unexpected invocation: inspect(1, 2, foo)", test_result.failure_message_lines[0]
  end
end