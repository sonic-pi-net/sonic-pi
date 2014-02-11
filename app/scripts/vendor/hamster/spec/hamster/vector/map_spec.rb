require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do

  [:map, :collect].each do |method|

    describe "##{method}" do

      describe "when empty" do

        before do
          @original = Hamster.vector
          @mapped = @original.send(method) {}
        end

        it "returns self" do
          @mapped.should equal(@original)
        end

      end

      describe "when not empty" do

        before do
          @original = Hamster.vector("A", "B", "C")
        end

        describe "with a block" do

          before do
            @mapped = @original.send(method, &:downcase)
          end

          it "preserves the original values" do
            @original.should == Hamster.vector("A", "B", "C")
          end

          it "returns a new vector with the mapped values" do
            @mapped.should == Hamster.vector("a", "b", "c")
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
