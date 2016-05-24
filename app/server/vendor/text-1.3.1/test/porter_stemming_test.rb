require_relative "./test_helper"
require "text/porter_stemming"

class PorterStemmingTest < Test::Unit::TestCase

  def test_cases
    inputs  = data_file('porter_stemming_input.txt').split(/\n/)
    outputs = data_file('porter_stemming_output.txt').split(/\n/)

    inputs.zip(outputs).each do |word, expected_output|
      assert_equal expected_output, Text::PorterStemming.stem(word)
    end
  end

end
