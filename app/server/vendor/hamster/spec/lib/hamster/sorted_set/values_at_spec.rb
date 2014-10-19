require "spec_helper"
require "hamster/sorted_set"

describe Hamster::SortedSet do
  describe "#values_at" do
    let(:sorted_set) { Hamster.sorted_set('a', 'b', 'c') }

    it "accepts any number of indices, and returns a sorted_set of items at those indices" do
      sorted_set.values_at(0).should eql(Hamster.sorted_set('a'))
      sorted_set.values_at(1,2).should eql(Hamster.sorted_set('b', 'c'))
    end

    context "when passed invalid indices" do
      it "filters them out" do
        sorted_set.values_at(1,2,3).should eql(Hamster.sorted_set('b', 'c'))
        sorted_set.values_at(-10,10).should eql(Hamster.sorted_set)
      end
    end

    context "when passed no arguments" do
      it "returns an empty sorted_set" do
        sorted_set.values_at.should eql(Hamster.sorted_set)
      end
    end

    context "from a subclass" do
      it "returns an instance of the subclass" do
        subclass = Class.new(Hamster::SortedSet)
        instance = subclass.new([1,2,3])
        instance.values_at(1,2).class.should be(subclass)
      end
    end
  end
end