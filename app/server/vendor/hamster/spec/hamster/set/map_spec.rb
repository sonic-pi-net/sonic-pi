require "spec_helper"

require "hamster/set"

describe Hamster::Set do

  [:map, :collect].each do |method|

    describe "##{method}" do

      describe "when empty" do

        before do
          @original = Hamster.set
          @mapped = @original.send(method) {}
        end

        it "returns self" do
          @mapped.should equal(@original)
        end

      end

      describe "when not empty" do

        before do
          @original = Hamster.set("A", "B", "C")
        end

        describe "with a block" do

          before do
            @mapped = @original.send(method, &:downcase)
          end

          it "preserves the original values" do
            @original.should == Hamster.set("A", "B", "C")
          end

          it "returns a new set with the mapped values" do
            @mapped.should == Hamster.set("a", "b", "c")
          end

        end

        describe "with no block" do

          before do
            @result = @original.send(method)
          end

          it "returns self" do
            @result.should equal(@original)
          end

        end

      end

    end

  end

end
