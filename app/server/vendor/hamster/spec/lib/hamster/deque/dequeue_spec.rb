require "spec_helper"
require "hamster/deque"

describe Hamster::Deque do
  [:dequeue, :tail, :shift].each do |method|
    describe "##{method}" do
      [
        [[], []],
        [["A"], []],
        [%w[A B C], %w[B C]],
      ].each do |values, expected|
        context "on #{values.inspect}" do
          let(:deque) { Hamster.deque(*values) }

          it "preserves the original" do
            deque.send(method)
            deque.should eql(Hamster.deque(*values))
          end

          it "returns #{expected.inspect}" do
            deque.send(method).should eql(Hamster.deque(*expected))
          end
        end
      end
    end
  end
end