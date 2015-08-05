require "spec_helper"
require "hamster/deque"

describe Hamster::Deque do
  describe "#unshift" do
    [
      [[], "A", ["A"]],
      [["A"], "B", %w[B A]],
      [["A"], "A", %w[A A]],
      [%w[A B C], "D", %w[D A B C]],
    ].each do |values, new_value, expected|
      context "on #{values.inspect} with #{new_value.inspect}" do
        let(:deque) { Hamster.deque(*values) }

        it "preserves the original" do
          deque.unshift(new_value)
          deque.should eql(Hamster.deque(*values))
        end

        it "returns #{expected.inspect}" do
          deque.unshift(new_value).should eql(Hamster.deque(*expected))
        end
      end
    end
  end
end