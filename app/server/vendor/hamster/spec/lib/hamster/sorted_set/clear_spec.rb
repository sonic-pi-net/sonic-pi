require "spec_helper"
require "hamster/sorted_set"

describe Hamster::SortedSet do
  describe "#clear" do
    [
      [],
      ["A"],
      %w[A B C],
    ].each do |values|
      context "on #{values}" do
        let(:sorted_set) { Hamster.sorted_set(*values) }

        it "preserves the original" do
          sorted_set.clear
          sorted_set.should eql(Hamster.sorted_set(*values))
        end

        it "returns an empty set" do
          sorted_set.clear.should equal(Hamster::EmptySortedSet)
          sorted_set.clear.should be_empty
        end
      end
    end

    context "from a subclass" do
      it "returns an empty instance of the subclass" do
        subclass = Class.new(Hamster::SortedSet)
        instance = subclass.new([:a, :b, :c, :d])
        instance.clear.class.should be(subclass)
        instance.clear.should be_empty
      end
    end
  end
end