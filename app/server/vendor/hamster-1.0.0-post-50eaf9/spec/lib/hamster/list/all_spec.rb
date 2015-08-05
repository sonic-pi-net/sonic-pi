require "spec_helper"
require "hamster/list"

describe Hamster::List do
  [:all?, :forall?].each do |method|
    describe "##{method}" do
      context "on a really big list" do
        let(:list) { Hamster.interval(0, STACK_OVERFLOW_DEPTH) }

        it "doesn't run out of stack" do
          -> { list.send(method) }.should_not raise_error
        end
      end

      context "when empty" do
        it "with a block returns true" do
          Hamster.list.send(method) {}.should == true
        end

        it "with no block returns true" do
          Hamster.list.send(method).should == true
        end
      end

      context "when not empty" do
        context "with a block" do
          let(:list) { Hamster.list("A", "B", "C") }

          context "if the block always returns true" do
            it "returns true" do
              list.send(method) { |item| true }.should == true
            end
          end

          context "if the block ever returns false" do
            it "returns false" do
              list.send(method) { |item| item == "D" }.should == false
            end
          end
        end

        context "with no block" do
          context "if all values are truthy" do
            it "returns true" do
              Hamster.list(true, "A").send(method).should == true
            end
          end

          [nil, false].each do |value|
            context "if any value is #{value.inspect}" do
              it "returns false" do
                Hamster.list(value, true, "A").send(method).should == false
              end
            end
          end
        end
      end
    end
  end
end