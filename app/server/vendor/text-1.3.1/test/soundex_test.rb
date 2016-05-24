require_relative "./test_helper"
require "text/soundex"
require 'yaml'

class SoundexTest < Test::Unit::TestCase

  def test_cases
    YAML.load(data_file('soundex.yml')).each do |input, expected_output|
      assert_equal expected_output, Text::Soundex.soundex(input)
    end
  end

  def test_should_return_nil_for_empty_string
    assert_nil Text::Soundex.soundex("")
  end

  def test_should_return_nil_for_string_with_no_letters
    assert_nil Text::Soundex.soundex("!@#123")
  end
end
