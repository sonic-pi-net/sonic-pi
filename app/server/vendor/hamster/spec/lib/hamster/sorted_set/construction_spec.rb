require "spec_helper"
require "hamster/sorted_set"

describe Hamster::SortedSet do
  describe ".sorted_set" do
    context "with no values" do
      it "returns the empty set" do
        Hamster.sorted_set.should equal(Hamster::EmptySortedSet)
        Hamster.sorted_set.should be_empty
      end
    end

    context "with a list of values" do
      let(:sorted_set) { Hamster.sorted_set("A", "B", "C") }

      it "is equivalent to repeatedly using #add" do
        sorted_set.size.should be(3)
        sorted_set.should eql(Hamster.sorted_set.add("A").add("B").add("C"))
      end
    end

    context "with a block" do
      it "returns a set sorted using the given block" do
        set = Hamster.sorted_set('fling', 'chalk', 'whip', 'papaya') { |str| str.reverse }
        set.to_a.should == ['papaya', 'fling', 'chalk', 'whip']
      end
    end
  end
end