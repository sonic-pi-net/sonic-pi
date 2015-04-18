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

  def test_message
    assert_match "Y U SO SLOW?", @error.to_s
    assert_match "Y U SO SLOW?", @error.message
  end
end

class IgnoreCallersTest < Minitest::Test
  class Boomer
    def initialize(*)
      raise Exception, "finder was created when it shouldn't!"
    end
  end

  def setup
    @org = DidYouMean.finders["NameError"]
    DidYouMean.finders["NameError"] = Boomer

    @error = assert_raises(NameError){ doesnt_exist }
  end

  def teardown
    DidYouMean.finders["NameError"] = @org
  end

  def test_ignore_missing_name
    assert_nothing_raised { missing_name }
  end

  def test_ignore_safe_constantize
    assert_nothing_raised { safe_constantize }
  end

  private

  def safe_constantize
    @error.message
  end

  def missing_name
    @error.message
  end

  def assert_nothing_raised
    yield
  end
end
