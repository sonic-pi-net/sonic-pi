require_relative 'test_helper'

class NameErrorExtensionTest < Minitest::Test
  class TestFinder
    def initialize(*); end
    def did_you_mean?; "Y U SO SLOW?"; end
  end

  def setup
    @old_finder = DidYouMean.finders["NameError"]
    DidYouMean.finders["NameError"] = TestFinder

    @error = assert_raises(NameError){ doesnt_exist }
  end

  def teardown
    DidYouMean.finders["NameError"] = @old_finder
  end

  def test_message?
    assert_match "Y U SO SLOW?", @error.to_s
    assert_match "Y U SO SLOW?", @error.message
  end
end
