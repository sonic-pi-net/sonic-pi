require "spec_helper"

require "hamster/list"

describe Hamster do

  describe "#cycle" do

    it "is lazy" do
      -> { Hamster.stream { fail }.cycle }.should_not raise_error
    end

    describe "with an empty list" do

      before do
        original = Hamster.list
        @result = original.cycle
      end

      it "returns an empty list" do
        @result.should be_empty
      end

    end

    describe "with a non-empty list" do

      before do
        @original = Hamster.list("A", "B", "C")
        @result = @original.cycle
      end

      it "preserves the original" do
        @original.should == Hamster.list("A", "B", "C")
      end

      it "infinitely cycles through all values" do
        @result.take(7).should == Hamster.list("A", "B", "C", "A", "B", "C", "A")
      end

    end

  end

end
