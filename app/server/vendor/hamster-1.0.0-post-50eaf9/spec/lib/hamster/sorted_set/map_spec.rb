require "spec_helper"
require "hamster/sorted_set"

describe Hamster::SortedSet do
  [:map, :collect].each do |method|
    describe "##{method}" do
      context "when empty" do
        it "returns self" do
          Hamster.sorted_set.send(method) {}.should equal(Hamster.sorted_set)
        end
      end

      context "when not empty" do
        let(:sorted_set) { Hamster.sorted_set("A", "B", "C") }

        context "with a block" do
          it "preserves the original values" do
            sorted_set.send(method, &:downcase)
            sorted_set.should eql(Hamster.sorted_set("A", "B", "C"))
          end

          it "returns a new set with the mapped values" do
            sorted_set.send(method, &:downcase).should eql(Hamster.sorted_set("a", "b", "c"))
          end
        end

        context "with no block" do
          it "returns an Enumerator" do
            sorted_set.send(method).class.should be(Enumerator)
            sorted_set.send(method).each(&:downcase).should == Hamster.sorted_set('a', 'b', 'c')
          end
        end
      end
    end
  end
end