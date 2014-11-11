require_relative 'test_helper'

class SimilarAttributeFinderTest < Minitest::Test
  def setup
    @error = assert_raises(ActiveRecord::UnknownAttributeError) do
      User.new(flrst_name: "wrong flrst name")
    end
  end

  def test_similar_words
    assert_includes @error.finder.similar_words, "first_name"
  end

  def test_did_you_mean?
    assert_match "Did you mean? first_name: string", @error.did_you_mean?
  end
end
