require 'spec_helper'

module OccurrenceRangeSpec
  class Foo < Treetop::Runtime::SyntaxNode
  end

  describe "zero to two of a terminal symbol followed by a node class declaration and a block" do
    testing_expression '"foo"..2 <OccurrenceRangeSpec::Foo> { def a_method; end }'

    it "successfully parses epsilon, reporting a failure" do
      parse('') do |result|
        result.should_not be_nil
        result.should be_an_instance_of(Foo)
        result.should respond_to(:a_method)

        terminal_failures = parser.terminal_failures
        terminal_failures.size.should == 0
      end
    end

    it "successfully parses epsilon, returning an instance declared node class and recording a terminal failure" do
      parse('') do |result|
        result.should_not be_nil
        result.should be_an_instance_of(Foo)
        result.should respond_to(:a_method)

        terminal_failures = parser.terminal_failures
        terminal_failures.size.should == 0
      end
    end

    it "successfully parses one of that terminal, returning an instance of the declared node class and recording a terminal failure" do
      parse("foo") do |result|
        result.should_not be_nil
        result.should be_an_instance_of(Foo)
        result.should respond_to(:a_method)

        terminal_failures = parser.terminal_failures
        terminal_failures.size.should == 0
      end
    end

    it "successfully parses two of that terminal, returning an instance of the declared node class and reporting no failure" do
      parse("foofoo") do |result|
        result.should_not be_nil
        result.should be_an_instance_of(Foo)
        result.should respond_to(:a_method)

        terminal_failures = parser.terminal_failures
        terminal_failures.size.should == 0
      end
    end

    it "fails to parses two of that terminal but fails because of an extra one" do
      parse("foofoofoo") do |result|
        result.should be_nil

        parser.terminal_failures.size.should == 1
      end
    end

    it "parses two of three of that terminal, reporting no failure" do
      parse("foofoofoo", :consume_all_input => false) do |result|
        result.should_not be_nil
        result.elements.size.should == 2

        parser.terminal_failures.size.should == 0
      end
    end

  end

  describe "two to four of a terminal symbol followed by a node class declaration and a block" do
    testing_expression '"foo" 2..4 <OccurrenceRangeSpec::Foo> { def a_method; end }'

    it "fails to parse epsilon, reporting a failure" do
      parse('') do |result|
        result.should be_nil
        terminal_failures = parser.terminal_failures
        terminal_failures.size.should == 1
        failure = terminal_failures.first
        failure.index.should == 0
        failure.expected_string.should == '"foo"'
      end
    end

    it "fails to parse one of that terminal, returning an instance of the declared node class and recording a terminal failure" do
      parse("foo") do |result|
        result.should be_nil

        terminal_failures = parser.terminal_failures
        terminal_failures.size.should == 1
        failure = terminal_failures.first
        failure.index.should == 3
        failure.expected_string.should == '"foo"'
      end
    end

    it "successfully parses two of that terminal, returning an instance of the declared node class and reporting no failure" do
      parse("foofoo") do |result|
        result.should_not be_nil
        result.should be_an_instance_of(Foo)
        result.should respond_to(:a_method)

        terminal_failures = parser.terminal_failures
        terminal_failures.size.should == 0
      end
    end

    it "successfully parses four of that terminal, returning an instance of the declared node class and reporting no failure" do
      parse("foofoofoofoo") do |result|
        result.should_not be_nil
        result.should be_an_instance_of(Foo)
        result.should respond_to(:a_method)

        terminal_failures = parser.terminal_failures
        terminal_failures.size.should == 0
      end
    end

    it "fails to parses four of that terminal because there's an extra unconsumed one" do
      parse("foofoofoofoofoo") do |result|
        result.should be_nil

        terminal_failures = parser.terminal_failures
        terminal_failures.size.should == 1
      end
    end
  end

  describe "two to any number of a terminal symbol followed by a node class declaration and a block" do
    testing_expression '"foo" 2.. <OccurrenceRangeSpec::Foo> { def a_method; end }'

    it "fails to parse epsilon, reporting a failure" do
      parse('') do |result|
        result.should be_nil
        terminal_failures = parser.terminal_failures
        terminal_failures.size.should == 1
        failure = terminal_failures.first
        failure.index.should == 0
        failure.expected_string.should == '"foo"'
      end
    end

    it "fails to parse one of that terminal, returning an instance of the declared node class and recording a terminal failure" do
      parse("foo") do |result|
        result.should be_nil

        terminal_failures = parser.terminal_failures
        terminal_failures.size.should == 1
        failure = terminal_failures.first
        failure.index.should == 3
        failure.expected_string.should == '"foo"'
      end
    end

    it "successfully parses two of that terminal, returning an instance of the declared node class and reporting no failure" do
      parse("foofoo") do |result|
        result.should_not be_nil
        result.should be_an_instance_of(Foo)
        result.should respond_to(:a_method)

        terminal_failures = parser.terminal_failures
        terminal_failures.size.should == 1
        failure = terminal_failures.first
        failure.index.should == 6
        failure.expected_string.should == '"foo"'
      end
    end

    it "successfully parses four of that terminal, returning an instance of the declared node class and reporting a failure on the fifth" do
      parse("foofoofoofoo") do |result|
        result.should_not be_nil
        result.should be_an_instance_of(Foo)
        result.should respond_to(:a_method)

        terminal_failures = parser.terminal_failures
        terminal_failures.size.should == 1
        failure = terminal_failures.first
        failure.index.should == 12
        failure.expected_string.should == '"foo"'
      end
    end
  end

end
