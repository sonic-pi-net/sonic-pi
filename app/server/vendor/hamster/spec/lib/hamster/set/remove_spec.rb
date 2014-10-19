require "spec_helper"
require "hamster/set"

describe Hamster::Set do
  [:remove, :reject, :delete_if].each do |method|
    describe "##{method}" do
      let(:set) { Hamster.set("A", "B", "C") }

      context "when nothing matches" do
        it "returns self" do
          set.send(method) { |item| false }.should equal(set)
        end
      end

      context "when only some things match" do
        context "with a block" do
          let(:result) { set.send(method) { |item| item == "A" }}

          it "preserves the original" do
            result
            set.should eql(Hamster.set("A", "B", "C"))
          end

          it "returns a set with the matching values" do
            result.should eql(Hamster.set("B", "C"))
          end
        end

        context "with no block" do
          it "returns self" do
            set.send(method).class.should be(Enumerator)
            set.send(method).each { |item| item == "A" }.should == Hamster.set("B", "C")
          end
        end
      end

      context "on a large set, with many combinations of input" do
        it "still works" do
          array = (1..1000).to_a
          set   = Hamster::Set.new(array)
          [0, 10, 100, 200, 500, 800, 900, 999, 1000].each do |threshold|
            result = set.send(method) { |item| item > threshold }
            result.size.should == threshold
            1.upto(threshold)  { |n| result.include?(n).should == true }
            (threshold+1).upto(1000) { |n| result.include?(n).should == false }
          end
        end
      end
    end
  end
end