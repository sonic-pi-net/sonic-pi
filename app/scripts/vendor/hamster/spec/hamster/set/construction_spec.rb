require "spec_helper"

require "hamster/set"

describe Hamster::Set do

  describe ".set" do

    describe "with no values" do

      it "returns the empty set" do
        Hamster.set.should equal(Hamster::EmptySet)
      end

    end

    describe "with a list of values" do

      before do
        @set = Hamster.set("A", "B", "C")
      end

      it "is equivalent to repeatedly using #add" do
        @set.should == Hamster.set.add("A").add("B").add("C")
      end

    end

  end

end
