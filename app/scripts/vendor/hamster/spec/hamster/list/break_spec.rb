require "spec_helper"

require "hamster/tuple"
require "hamster/list"

describe Hamster::List do

  describe "#break" do

    it "is lazy" do
      -> { Hamster.stream { fail }.break { |item| false } }.should_not raise_error
    end

    [
      [[], [], []],
      [[1], [1], []],
      [[1, 2], [1, 2], []],
      [[1, 2, 3], [1, 2], [3]],
      [[1, 2, 3, 4], [1, 2], [3, 4]],
      [[2, 3, 4], [2], [3, 4]],
      [[3, 4], [], [3, 4]],
      [[4], [], [4]],
    ].each do |values, expected_prefix, expected_remainder|

      describe "on #{values.inspect}" do

        before do
          @original = Hamster.list(*values)
        end

        describe "with a block" do

          before do
            @result = @original.break { |item| item > 2 }
            @prefix = @result.first
            @remainder = @result.last
          end

          it "preserves the original" do
            @original.should == Hamster.list(*values)
          end

          it "returns a tuple with two items" do
            @result.is_a?(Hamster::Tuple).should == true
          end

          it "correctly identifies the prefix" do
            @prefix.should == Hamster.list(*expected_prefix)
          end

          it "correctly identifies the remainder" do
            @remainder.should == Hamster.list(*expected_remainder)
          end

        end

        describe "without a block" do

          before do
            @result = @original.break
            @prefix = @result.first
            @remainder = @result.last
          end

          it "returns a tuple with two items" do
            @result.is_a?(Hamster::Tuple).should == true
          end

          it "returns self as the prefix" do
            @prefix.should equal(@original)
          end

          it "leaves the remainder empty" do
            @remainder.should be_empty
          end

        end

      end

    end

  end

end
