require File.expand_path('../acceptance_test_helper', __FILE__)
require 'mocha/setup'

class Bug21465Test < Mocha::TestCase

  include AcceptanceTest

  def setup
    setup_acceptance_test
  end

  def teardown
    teardown_acceptance_test
  end

  def test_should_allow_expected_method_name_to_be_a_string
    test_result = run_as_test do
      mock = mock()
      mock.expects('wibble')
      mock.wibble
    end
    assert_passed(test_result)
  end

  def test_should_allow_stubbed_method_name_to_be_a_string
    test_result = run_as_test do
      mock = mock()
      mock.stubs('wibble')
      mock.wibble
    end
    assert_passed(test_result)
  end

end
