require "spec_helper"
require "hamster/list"

describe Hamster::List do
  [:empty?, :null?].each do |method|
    describe "##{method}" do
      context "on a really big list" do
        it "doesn't run out of stack" do
          -> { Hamster.interval(0, STACK_OVERFLOW_DEPTH).filter(&:nil?).empty? }.should_not raise_error
        end
      end

      [
        [[], true],
        [["A"], false],
        [%w[A B C], false],
      ].each do |values, expected|
        context "on #{values.inspect}" do
          it "returns #{expected.inspect}" do
            Hamster.list(*values).send(method).should == expected
          end
        end
      end
    end
  end
end