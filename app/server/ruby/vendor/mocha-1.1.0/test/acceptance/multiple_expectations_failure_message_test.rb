require File.expand_path('../acceptance_test_helper', __FILE__)
require 'mocha/setup'

class FailureMessageTest < Mocha::TestCase

  include AcceptanceTest

  def setup
    setup_acceptance_test
  end

  def teardown
    teardown_acceptance_test
  end

  def test_should_include_unexpected_invocation_in_unsatisfied_expectation_message
    test_result = run_as_test do
      mock = mock('mock')
      mock.expects(:method_one).once
      2.times { mock.method_one }
    end
    assert_failed(test_result)
    assert_equal [
      "unexpected invocation: #<Mock:mock>.method_one()",
      "unsatisfied expectations:",
      "- expected exactly once, invoked twice: #<Mock:mock>.method_one(any_parameters)"
     ], test_result.failure_message_lines
  end

  def test_should_report_satisfied_expectations_as_well_as_unsatisfied_expectations
    test_result = run_as_test do
      mock = mock('mock')
      mock.expects(:method_one).once
      mock.expects(:method_two).twice
      1.times { mock.method_one }
      1.times { mock.method_two }
    end
    assert_failed(test_result)
    assert_equal [
      "not all expectations were satisfied",
       "unsatisfied expectations:",
       "- expected exactly twice, invoked once: #<Mock:mock>.method_two(any_parameters)",
       "satisfied expectations:",
       "- expected exactly once, invoked once: #<Mock:mock>.method_one(any_parameters)"
    ], test_result.failure_message_lines
  end

  def test_should_report_multiple_satisfied_expectations
    test_result = run_as_test do
      mock = mock('mock')
      mock.expects(:method_one).once
      mock.expects(:method_two).twice
      mock.expects(:method_three).times(3)
      1.times { mock.method_one }
      2.times { mock.method_two }
      2.times { mock.method_three }
    end
    assert_failed(test_result)
    assert_equal [
      "not all expectations were satisfied",
      "unsatisfied expectations:",
      "- expected exactly 3 times, invoked twice: #<Mock:mock>.method_three(any_parameters)",
      "satisfied expectations:",
      "- expected exactly twice, invoked twice: #<Mock:mock>.method_two(any_parameters)",
      "- expected exactly once, invoked once: #<Mock:mock>.method_one(any_parameters)"
     ], test_result.failure_message_lines
  end
end
