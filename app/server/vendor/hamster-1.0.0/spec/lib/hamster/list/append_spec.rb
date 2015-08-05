require "spec_helper"
require "hamster/list"

describe Hamster::List do
  [:append, :concat, :+].each do |method|
    describe "##{method}" do
      it "is lazy" do
        -> { Hamster.stream { fail }.append(Hamster.stream { fail }) }.should_not raise_error
      end

      [
        [[], [], []],
        [["A"], [], ["A"]],
        [[], ["A"], ["A"]],
        [%w[A B], %w[C D], %w[A B C D]],
      ].each do |left_values, right_values, expected|
        context "on #{left_values.inspect} and #{right_values.inspect}" do
          let(:left) { Hamster.list(*left_values) }
          let(:right) { Hamster.list(*right_values) }
          let(:result) { left.append(right) }

          it "preserves the left" do
            result
            left.should eql(Hamster.list(*left_values))
          end

          it "preserves the right" do
            result
            right.should eql(Hamster.list(*right_values))
          end

          it "returns #{expected.inspect}" do
            result.should eql(Hamster.list(*expected))
          end
        end
      end
    end
  end
end