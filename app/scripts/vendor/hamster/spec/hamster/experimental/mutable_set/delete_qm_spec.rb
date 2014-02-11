require "spec_helper"

require "hamster/experimental/mutable_set"

describe Hamster::Set do

  describe "#delete?" do

    before do
      @set = Hamster.mutable_set("A", "B", "C")
    end

    describe "with an existing value" do

      before do
        @result = @set.delete?("B")
      end

      it "returns true" do
        @result.should == true
      end

      it "modifies the set to remove the value" do
        @set.should == Hamster.mutable_set("A", "C")
      end

    end

    describe "with a non-existing value" do

      before do
        @result = @set.delete?("D")
      end

      it "preserves the original values" do
        @set.should == Hamster.mutable_set("A", "B", "C")
      end

      it "returns false" do
        @result.should == false
      end

    end

  end

end
