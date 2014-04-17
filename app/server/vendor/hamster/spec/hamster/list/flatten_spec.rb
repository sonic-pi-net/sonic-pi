require "spec_helper"

require "hamster/list"

describe Hamster do

  describe "#flatten" do

    it "is lazy" do
      -> { Hamster.stream { fail }.flatten }.should_not raise_error
    end

    [
      [[], []],
      [["A"], ["A"]],
      [%w[A B C], %w[A B C]],
      [["A", Hamster.list("B"), "C"], %w[A B C]],
      [[Hamster.list("A"), Hamster.list("B"), Hamster.list("C")], %w[A B C]],
    ].each do |values, expected|

      describe "on #{values}" do

        before do
          @original = Hamster.list(*values)
          @result = @original.flatten
        end

        it "preserves the original" do
          @original.should == Hamster.list(*values)
        end

        it "returns an empty list" do
          @result.should == Hamster.list(*expected)
        end

      end

    end

  end

end
