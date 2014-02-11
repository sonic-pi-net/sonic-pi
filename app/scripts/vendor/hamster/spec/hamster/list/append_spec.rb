require "spec_helper"

require "hamster/list"

describe Hamster::List do

  [:append, :concat, :cat, :+].each do |method|

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

        describe "on #{left_values.inspect} and #{right_values.inspect}" do

          before do
            @left = Hamster.list(*left_values)
            @right = Hamster.list(*right_values)
            @result = @left.append(@right)
          end

          it "preserves the left" do
            @left.should == Hamster.list(*left_values)
          end

          it "preserves the right" do
            @right.should == Hamster.list(*right_values)
          end

          it "returns #{expected.inspect}" do
            @result.should == Hamster.list(*expected)
          end

        end

      end

    end

  end

end
