require "spec_helper"

require "hamster/tuple"
require "hamster/list"

describe Hamster::List do

  describe "#partition" do

    it "is lazy" do
      -> { Hamster.stream { fail }.partition }.should_not raise_error
    end

    [
      [[], [], []],
      [[1], [1], []],
      [[1, 2], [1], [2]],
      [[1, 2, 3], [1, 3], [2]],
      [[1, 2, 3, 4], [1, 3], [2, 4]],
      [[2, 3, 4], [3], [2, 4]],
      [[3, 4], [3], [4]],
      [[4], [], [4]],
    ].each do |values, expected_matches, expected_remainder|

      describe "on #{values.inspect}" do

        before do
          @original = Hamster.list(*values)
        end

        describe "with a block" do

          before do
            @result = @original.partition(&:odd?)
            @matches = @result.first
            @remainder = @result.last
          end

          it "preserves the original" do
            @original.should == Hamster.list(*values)
          end

          it "returns a tuple with two items" do
            @result.is_a?(Hamster::Tuple).should == true
          end

          it "correctly identifies the matches" do
            @matches.should == Hamster.list(*expected_matches)
          end

          it "correctly identifies the remainder" do
            @remainder.should == Hamster.list(*expected_remainder)
          end

        end

        describe "without a block" do

          before do
            @result = @original.partition
          end

          it "returns self" do
            @result.should equal(@original)
          end

        end

      end

    end

  end

end
