require 'spec_helper'

module TerminalSymbolSpec
  class Foo < Treetop::Runtime::SyntaxNode
  end

  describe "a terminal symbol" do
    testing_expression "'Foo'"

    it "matches the input string" do
      parse "Foo", :index => 0 do |result|
        result.should_not be_nil
        result.interval.should == (0...3)
        result.text_value.should == 'Foo'
      end
    end

    it "fails to match the input string other than at the start" do
      parse " Foo", :index => 0 do |result|
        result.should be_nil
        parser.terminal_failures.size.should == 1
      end
    end

    it "fails to match the input string in the wrong case" do
      parse "foo", :index => 0 do |result|
        result.should be_nil
        parser.terminal_failures.size.should == 1
      end
    end
  end

  describe "a terminal symbol with case-insensitive matching" do
    testing_expression "'Foo'i"

    it "matches the input string in the same case" do
      parse "Foo", :index => 0 do |result|
        result.should_not be_nil
        result.interval.should == (0...3)
        result.text_value.should == 'Foo'
      end
    end

    it "matches the input string in varied case" do
      parse "foO", :index => 0 do |result|
        result.should_not be_nil
        result.interval.should == (0...3)
        result.text_value.should == 'foO'
      end
    end
  end

  describe "a terminal symbol followed by a node class declaration and a block" do
    testing_expression "'foo' <TerminalSymbolSpec::Foo> { def a_method; end }"

    it "correctly parses matching input prefixes at various indices, returning an instance of the declared class that can respond to methods defined in the inline module" do
      parse "foo", :index => 0 do |result|
        result.should be_an_instance_of(Foo)
        result.should respond_to(:a_method)
        result.interval.should == (0...3)
        result.text_value.should == 'foo'
      end

      parse "xfoo", :index => 1 do |result|
        result.should be_an_instance_of(Foo)
        result.should respond_to(:a_method)
        result.interval.should == (1...4)
        result.text_value.should == 'foo'
      end
    
      parse "---foo", :index => 3 do |result|
        result.should be_an_instance_of(Foo)
        result.should respond_to(:a_method)
        result.interval.should == (3...6)
        result.text_value.should == 'foo'
      end
    end

    it "fails to parse nonmatching input at the index even if a match occurs later" do
      parse(" foo", :index =>  0) do |result|
        result.should be_nil
        parser.terminal_failures.size.should == 1
      end
    end
  end

  module ModFoo
  end

  describe "a terminal symbol followed by a node class declaration and a block" do
    testing_expression "'foo' <TerminalSymbolSpec::ModFoo> { def a_method; end }"

    it "correctly parses matching input prefixes at various indices, returning an instance of SyntaxNode extended with the declared module that can respond to methods defined in the inline module" do
      parse "foo", :index => 0 do |result|
        result.should be_an_instance_of(Treetop::Runtime::SyntaxNode)
        result.should be_a_kind_of(ModFoo)
        result.should respond_to(:a_method)
        result.interval.should == (0...3)
        result.text_value.should == 'foo'
      end

      parse "xfoo", :index => 1 do |result|
        result.should be_an_instance_of(Treetop::Runtime::SyntaxNode)
        result.should be_a_kind_of(ModFoo)
        result.should respond_to(:a_method)
        result.interval.should == (1...4)
        result.text_value.should == 'foo'
      end
    
      parse "---foo", :index => 3 do |result|
        result.should be_an_instance_of(Treetop::Runtime::SyntaxNode)
        result.should be_a_kind_of(ModFoo)
        result.should respond_to(:a_method)
        result.interval.should == (3...6)
        result.text_value.should == 'foo'
      end
    end
  end

  describe "a terminal regexp" do
    testing_expression "'Fo+'r"

    it "matches the input string" do
      parse "Fooo", :index => 0 do |result|
        result.should_not be_nil
        result.interval.should == (0...4)
        result.text_value.should == 'Fooo'
      end
    end

    it "fails to match the input string other than at the start" do
      parse " Foo", :index => 0 do |result|
        result.should be_nil
        parser.terminal_failures.size.should == 1
      end
    end

    it "fails to match the input string in the wrong case" do
      parse "foo", :index => 0 do |result|
        result.should be_nil
        parser.terminal_failures.size.should == 1
      end
    end
  end

  describe "a case-insensitive terminal regexp" do
    testing_expression "'Fo+'ri"

    it "matches the input string in the same case" do
      parse "Fooo", :index => 0 do |result|
        result.should_not be_nil
        result.interval.should == (0...4)
        result.text_value.should == 'Fooo'
      end
    end

    it "matches the input string in the same case" do
      parse "foOo", :index => 0 do |result|
        result.should_not be_nil
        result.interval.should == (0...4)
        result.text_value.should == 'foOo'
      end
    end
  end

#  Transient symbols were part of some idea of Nathan's that I no longer recall
#  describe "a transient terminal symbol" do
#    testing_expression "~'foo'"
#
#    it "returns true upon parsing matching input prefixes at various indices" do
#      pending "transient terminal expressions"
#      parse("foo", :index => 0).should be_truthy
#      parse("-foo", :index => 1).should be_truthy
#      parse("---foo", :index => 3).should be_truthy
#    end
#  end
end
