require "spec_helper"
require "hamster/list"

describe Hamster::List do
  describe "#zip" do
    it "is lazy" do
      -> { Hamster.stream { fail }.zip(Hamster.stream { fail }) }.should_not raise_error
    end

    [
      [[], [], []],
      [["A"], ["aye"], [Hamster.list("A", "aye")]],
      [["A"], [], [Hamster.list("A", nil)]],
      [[], ["A"], [Hamster.list(nil, "A")]],
      [%w[A B C], %w[aye bee see], [Hamster.list("A", "aye"), Hamster.list("B", "bee"), Hamster.list("C", "see")]],
    ].each do |left, right, expected|

      describe "on #{left.inspect} and #{right.inspect}" do
        before do
          @left = Hamster.list(*left)
          @right = Hamster.list(*right)
          @result = @left.zip(@right)
        end

        it "returns #{expected.inspect}" do
          @result.should == Hamster.list(*expected)
        end
      end
    end
  end
end
