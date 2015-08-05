require "spec_helper"
require "hamster/sorted_set"

describe Hamster::SortedSet do
  [:each, :foreach].each do |method|
    describe "##{method}" do
      context "with no block" do
        let(:sorted_set) { Hamster.sorted_set("A", "B", "C") }

        it "returns an Enumerator" do
          sorted_set.send(method).class.should be(Enumerator)
          sorted_set.send(method).to_a.should eql(sorted_set.to_a)
        end
      end

      context "with a block" do
        let(:sorted_set) { Hamster::SortedSet.new((1..1025).to_a.reverse) }

        it "returns self" do
          sorted_set.send(method) {}.should be(sorted_set)
        end

        it "iterates over the items in order" do
          items = []
          sorted_set.send(method) { |item| items << item }
          items.should == (1..1025).to_a
        end
      end
    end
  end
end