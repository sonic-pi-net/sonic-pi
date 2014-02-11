require "spec_helper"

require "hamster/experimental/mutable_set"

describe Hamster::MutableSet do

  describe "#add?" do

    before do
      @set = Hamster.mutable_set("A", "B", "C")
    end

    describe "with a unique value" do

      before do
        @result = @set.add?("D")
      end

      it "returns true" do
        @result.should == true
      end

      it "modifies the set to include the new value" do
        @set.should == Hamster.mutable_set("A", "B", "C", "D")
      end

    end

    describe "with a duplicate value" do

      before do
        @result = @set.add?("C")
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
