require "spec_helper"
require "hamster/deque"

describe Hamster::Deque do
  [:head, :first, :front].each do |method|
    describe "##{method}" do
      [
        [[], nil],
        [["A"], "A"],
        [%w[A B C], "A"],
      ].each do |values, expected|
        context "on #{values.inspect}" do
          it "returns #{expected.inspect}" do
            Hamster.deque(*values).send(method).should == expected
          end
        end
      end
    end
  end
end