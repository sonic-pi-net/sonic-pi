require "spec_helper"

require "hamster/list"

describe Hamster::List do

  [:all?, :forall?].each do |method|

    describe "##{method}" do

      describe "on a really big list" do

        before do
          @list = Hamster.interval(0, STACK_OVERFLOW_DEPTH)
        end

        it "doesn't run out of stack" do
          -> { @list.send(method) }.should_not raise_error
        end

      end

      describe "when empty" do

        before do
          @list = Hamster.list
        end

        it "with a block returns true" do
          @list.send(method) {}.should == true
        end

        it "with no block returns true" do
          @list.send(method).should == true
        end

      end

      describe "when not empty" do

        describe "with a block" do

          before do
            @list = Hamster.list("A", "B", "C")
          end

          describe "if the block always returns true" do

            before do
              @result = @list.send(method) { |item| true }
            end

            it "returns true" do
              @result.should == true
            end

          end

          describe "if the block ever returns false" do

            before do
              @result = @list.send(method) { |item| item == "D" }
            end

            it "returns false" do
              @result.should == false
            end

          end

        end

        describe "with no block" do

          describe "if all values are truthy" do

            before do
              @result = Hamster.list(true, "A").send(method)
            end

            it "returns true" do
              @result.should == true
            end

          end

          [nil, false].each do |value|

            describe "if any value is #{value.inspect}" do

              before do
                @result = Hamster.list(value, true, "A").send(method)
              end

              it "returns false" do
                @result.should == false
              end

            end

          end

        end

      end

    end

  end

end
