require_relative "./test_helper"
require "text/metaphone"
require 'yaml'

class MetaphoneTest < Test::Unit::TestCase

  def test_cases
    YAML.load(data_file('metaphone.yml')).each do |input, expected_output|
      assert_equal expected_output, Text::Metaphone.metaphone(input)
    end
  end

  def test_cases_for_buggy_implementation
    YAML.load(data_file('metaphone_buggy.yml')).each do |input, expected_output|
      assert_equal expected_output, Text::Metaphone.metaphone(input, :buggy=>true)
    end
  end

  def test_junk
    assert_equal Text::Metaphone.metaphone('foobar'),
      Text::Metaphone.metaphone('%^@#$^f%^&o%^o@b#a@#r%^^&')
    assert_equal Text::Metaphone.metaphone('foobar', :buggy=>true),
      Text::Metaphone.metaphone('%^@#$^f%^&o%^o@b#a@#r%^^&', :buggy=>true)
  end

  def test_caps
    assert_equal Text::Metaphone.metaphone('foobar'),
      Text::Metaphone.metaphone('FOOBAR')
    assert_equal Text::Metaphone.metaphone('foobar', :buggy=>true),
      Text::Metaphone.metaphone('FOOBAR', :buggy=>true)
  end

  def test_string
    assert_equal 'F BR BS', Text::Metaphone.metaphone('foo bar baz')
    assert_equal 'N WT', Text::Metaphone.metaphone('gnu what')
    assert_equal 'F BR BS', Text::Metaphone.metaphone('foo bar baz', :buggy=>true)
    assert_equal 'N WT', Text::Metaphone.metaphone('gnu what', :buggy=>true)
  end

end
