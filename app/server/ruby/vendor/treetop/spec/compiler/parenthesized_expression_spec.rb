require 'spec_helper'

module ParenthesizedExpressionSpec
  describe "An unadorned expression inside of parentheses" do
    testing_expression '("foo")'
  
    it "should behave as normal" do
      parse('foo').should_not be_nil
    end
  end

  describe "A prefixed-expression inside of parentheses" do
    testing_expression '(!"foo")'
  
    it "should behave as normal" do
      parse('foo') do |result|
        result.should be_nil
        parser.terminal_failures.size.should == 1
      end
    end
  end

  describe "An expression with code both inside and outside parentheses" do
    testing_expression '("foo" { def inner; end } ) { def outer; end} '
    it "should extend both code modules " do
      parse('foo') do |result|
        skip "Arbitrarily nested modules are not yet compiled"
        result.should respond_to(:inner)
        result.should respond_to(:outer)
      end
    end
  end

end
