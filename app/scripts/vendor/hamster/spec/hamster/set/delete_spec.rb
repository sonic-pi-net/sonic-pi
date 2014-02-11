require "spec_helper"

require "hamster/set"

describe Hamster::Set do

  describe "#delete" do

    before do
      @original = Hamster.set("A", "B", "C")
    end

    describe "with an existing value" do

      before do
        @result = @original.delete("B")
      end

      it "preserves the original" do
        @original.should == Hamster.set("A", "B", "C")
      end

      it "returns a copy with the remaining of values" do
        @result.should == Hamster.set("A", "C")
      end

    end

    describe "with a non-existing value" do

      before do
        @result = @original.delete("D")
      end

      it "preserves the original values" do
        @original.should == Hamster.set("A", "B", "C")
      end

      it "returns self" do
        @result.should equal(@original)
      end

    end

  end

end
