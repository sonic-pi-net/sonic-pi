require File.expand_path('../../test_helper', __FILE__)
require 'mocha/hooks'

class HooksTest < Mocha::TestCase
  class Mocha::Mockery
    class << self
      attr_writer :instance
    end
  end

  class FakeMockery
    def verify(*args)
    end

    def teardown
      raise "exception within Mockery#teardown"
    end
  end

  def test_ensure_mockery_instance_is_reset_even_when_an_exception_is_raised_in_mockery_teardown
    fake_test_case = Object.new.extend(Mocha::Hooks)
    original_mockery = FakeMockery.new
    Mocha::Mockery.instance = original_mockery

    fake_test_case.mocha_teardown rescue nil

    assert_not_same Mocha::Mockery.instance, original_mockery
  end
end
