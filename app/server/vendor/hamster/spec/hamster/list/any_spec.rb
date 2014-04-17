require "spec_helper"

require "hamster/list"

describe Hamster::List do

  [:any?, :exist?, :exists?].each do |method|

    describe "##{method}" do

      describe "on a really big list" do

        before do
          @list = Hamster.interval(0, STACK_OVERFLOW_DEPTH)
        end

        it "doesn't run out of stack" do
          -> { @list.send(method) { false } }.should_not raise_error
        end

      end

      describe "when empty" do

        before do
          @list = Hamster.list
        end

        it "with a block returns false" do
          @list.send(method) {}.should == false
        end

        it "with no block returns false" do
          @list.send(method).should == false
        end

      end

      describe "when not empty" do

        describe "with a block" do

          before do
            @list = Hamster.list("A", "B", "C", nil)
          end

          ["A", "B", "C", nil].each do |value|

            it "returns true if the block ever returns true (#{value.inspect})" do
              @list.send(method) { |item| item == value }.should == true
            end

          end

          it "returns false if the block always returns false" do
            @list.send(method) { |item| item == "D" }.should == false
          end

        end

        describe "with no block" do

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
