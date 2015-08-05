require "spec_helper"
require "hamster/deque"

describe Hamster::Deque do
  describe "#pop" do
    [
      [[], []],
      [["A"], []],
      [%w[A B C], %w[A B]],
    ].each do |values, expected|
      context "on #{values.inspect}" do
        let(:deque) { Hamster.deque(*values) }

        it "preserves the original" do
          deque.pop
          deque.should eql(Hamster.deque(*values))
        end

        it "returns #{expected.inspect}" do
          deque.pop.should eql(Hamster.deque(*expected))
        end
      end
    end
  end
end