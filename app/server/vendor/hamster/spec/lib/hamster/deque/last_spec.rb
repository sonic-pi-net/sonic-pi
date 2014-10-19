require "spec_helper"
require "hamster/deque"

describe Hamster::Deque do
  [:last, :peek].each do |method|
    describe "##{method}" do
      [
        [[], nil],
        [["A"], "A"],
        [%w[A B C], "C"],
      ].each do |values, expected|
        context "on #{values.inspect}" do
          it "returns #{expected.inspect}" do
            Hamster.deque(*values).send(method).should eql(expected)
          end
        end
      end
    end
  end
end