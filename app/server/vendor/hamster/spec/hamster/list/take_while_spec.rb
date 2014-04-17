require "spec_helper"

require "hamster/list"

describe Hamster::List do
  describe "#take_while" do
    it "is lazy" do
      -> { Hamster.stream { fail }.take_while { false } }.should_not raise_error
    end

    [
      [[], []],
      [["A"], ["A"]],
      [%w[A B C], %w[A B]],
    ].each do |values, expected|

      describe "on #{values.inspect}" do
        before do
          @original = Hamster.list(*values)
          @result = @original.take_while { |item| item < "C" }
        end

        describe "with a block" do
          it "returns #{expected.inspect}" do
            @result.should == Hamster.list(*expected)
          end

          it "preserves the original" do
            @original.should == Hamster.list(*values)
          end

          it "is lazy" do
            count = 0
            @original.take_while do |item|
              count += 1
              true
            end
            count.should <= 1
          end
        end

        describe "without a block" do
          before do
            @result = @original.take_while
          end

          it "returns self" do
            @result.should equal(@original)
          end
        end
      end
    end
  end
end
