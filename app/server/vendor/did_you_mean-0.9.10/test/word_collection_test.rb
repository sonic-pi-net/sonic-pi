require_relative 'test_helper'

class WordCollectionTest < Minitest::Test
  def setup
    names = %w(
      name12
      name123
      name1234
      name12345
      name123456
    )

    @word_collection = DidYouMean::WordCollection.new(names)
  end

  def test_similar_to
    expected = %w(
      name123456
      name12345
      name1234
      name123
    )

    assert_equal expected, @word_collection.similar_to("name123456")
  end
end
