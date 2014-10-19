require "spec_helper"
require "hamster/list"

describe Hamster::List do
  [:any?, :exist?, :exists?].each do |method|
    describe "##{method}" do
      context "on a really big list" do
        let(:list) { Hamster.interval(0, STACK_OVERFLOW_DEPTH) }

        it "doesn't run out of stack" do
          -> { list.send(method) { false } }.should_not raise_error
        end
      end

      context "when empty" do
        it "with a block returns false" do
         Hamster.list.send(method) {}.should == false
        end

        it "with no block returns false" do
          Hamster.list.send(method).should == false
        end
      end

      context "when not empty" do
        context "with a block" do
          let(:list) { Hamster.list("A", "B", "C", nil) }

          ["A", "B", "C", nil].each do |value|
            it "returns true if the block ever returns true (#{value.inspect})" do
              list.send(method) { |item| item == value }.should == true
            end
          end

          it "returns false if the block always returns false" do
            list.send(method) { |item| item == "D" }.should == false
          end
        end

        context "with no block" do
          it "returns true if any value is truthy" do
            Hamster.list(nil, false, "A", true).send(method).should == true
          end

          it "returns false if all values are falsey" do
            Hamster.list(nil, false).send(method).should == false
          end
        end
      end
    end
  end
end