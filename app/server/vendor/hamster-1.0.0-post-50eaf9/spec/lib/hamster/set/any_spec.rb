require "spec_helper"
require "hamster/set"

describe Hamster::Set do
  [:any?, :exist?, :exists?].each do |method|
    describe "##{method}" do
      context "when empty" do
        it "with a block returns false" do
          Hamster.set.send(method) {}.should == false
        end

        it "with no block returns false" do
          Hamster.set.send(method).should == false
        end
      end

      context "when not empty" do
        context "with a block" do
          let(:set) { Hamster.set("A", "B", "C", nil) }

          ["A", "B", "C", nil].each do |value|
            it "returns true if the block ever returns true (#{value.inspect})" do
              set.send(method) { |item| item == value }.should == true
            end
          end

          it "returns false if the block always returns false" do
            set.send(method) { |item| item == "D" }.should == false
          end

          it "propagates exceptions raised in the block" do
            -> { set.any? { |k,v| raise "help" } }.should raise_error(RuntimeError)
          end

          it "stops iterating as soon as the block returns true" do
            yielded = []
            set.any? { |k,v| yielded << k; true }
            yielded.size.should == 1
          end
        end

        context "with no block" do
          it "returns true if any value is truthy" do
            Hamster.set(nil, false, true, "A").send(method).should == true
          end

          it "returns false if all values are falsey" do
            Hamster.set(nil, false).send(method).should == false
          end
        end
      end
    end
  end
end