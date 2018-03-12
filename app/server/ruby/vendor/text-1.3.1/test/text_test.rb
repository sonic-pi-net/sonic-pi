require_relative "./test_helper"

class TextTest < Test::Unit::TestCase
  def test_should_load_all_components
    require 'text'
    assert defined? Text::Levenshtein
    assert defined? Text::Metaphone
    assert defined? Text::PorterStemming
    assert defined? Text::Soundex
    assert defined? Text::VERSION
    assert defined? Text::WhiteSimilarity
  end
end
