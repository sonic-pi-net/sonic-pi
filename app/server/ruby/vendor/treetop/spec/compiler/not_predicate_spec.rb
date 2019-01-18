require 'spec_helper'

module NotPredicateSpec
  describe "A !-predicated terminal symbol" do
    testing_expression '!"foo"'

    it "fails to parse input matching the terminal symbol" do
      parse('foo') do |result|
        result.should be_nil
        parser.terminal_failures.size.should == 1
      end
    end
  end

  describe "A !-predicated character class symbol" do
    testing_expression '![aeiou]'

    it "fails to parse input matching the terminal symbol" do
      parse('e') do |result|
        result.should be_nil
        parser.terminal_failures.size.should == 1
      end
    end
  end

  describe "A sequence of a terminal and an and another !-predicated terminal" do
    testing_expression '"foo" !"bar"'

    it "fails to match input matching both terminals" do
      parse('foobar').should be_nil
    end
  
    it "successfully parses input matching the first terminal and not the second, reporting the parse failure of the second terminal" do
      parse('foo') do |result|
        result.should_not be_nil
        terminal_failures = parser.terminal_failures
        terminal_failures.size.should == 0
      end
    end
  end

  describe "A !-predicated sequence" do
    testing_expression '!("a" "b" "cc")'

    it "fails to parse matching input" do
      parse('abcc') do |result|
        result.should be_nil
        parser.terminal_failures.size.should == 1
      end
    end
  end
end
