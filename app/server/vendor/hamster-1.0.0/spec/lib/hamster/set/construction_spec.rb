require "spec_helper"
require "hamster/set"

describe Hamster::Set do
  describe ".set" do
    context "with no values" do
      it "returns the empty set" do
        Hamster.set.should be_empty
        Hamster.set.should equal(Hamster::EmptySet)
      end
    end

    context "with a list of values" do
      it "is equivalent to repeatedly using #add" do
        Hamster.set("A", "B", "C").should eql(Hamster.set.add("A").add("B").add("C"))
      end
    end
  end
end