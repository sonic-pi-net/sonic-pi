require "spec_helper"

require "hamster/list"

describe "Hamster.list#span" do

  it "is lazy" do
    -> { Hamster.stream { |item| fail }.span { true } }.should_not raise_error
  end

  describe <<-DESC do
given a predicate (in the form of a block), splits the list into two lists
  (returned as a tuple) such that elements in the first list (the prefix) are
  taken from the head of the list while the predicate is satisfied, and elements
  in the second list (the remainder) are the remaining elements from the list
  once the predicate is not satisfied. For example:
DESC

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

      describe "given the list #{values.inspect}" do

        before do
          @original = Hamster.list(*values)
        end

        describe "and a predicate that returns true for values <= 2" do

          before do
            @result = @original.span { |item| item <= 2 }
            @prefix = @result.first
            @remainder = @result.last
          end

          it "preserves the original" do
            @original.should == Hamster.list(*values)
          end

          it "returns the prefix as #{expected_prefix.inspect}" do
            @prefix.should == Hamster.list(*expected_prefix)
          end

          it "returns the remainder as #{expected_remainder.inspect}" do
            @remainder.should == Hamster.list(*expected_remainder)
          end

        end

        describe "without a predicate" do

          before do
            @result = @original.span
            @prefix = @result.first
            @remainder = @result.last
          end

          it "returns a tuple" do
            @result.is_a?(Hamster::Tuple).should == true
          end

          it "returns self as the prefix" do
            @prefix.should equal(@original)
          end

          it "returns an empty list as the remainder" do
            @remainder.should be_empty
          end

        end

      end

    end

  end

end
