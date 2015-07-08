require_relative 'test_helper'

class NullFinderTest < Minitest::Test
  class FirstNameError < NameError; end

  def setup
    @error = assert_raises(FirstNameError) do
      raise FirstNameError, "Other name error"
    end
  end

  def test_did_you_mean?
    assert_nil @error.did_you_mean?
  end

  def test_message
    refute_includes "Did you mean?", @error.message
  end
end
