require "spec_helper"

require "hamster/set"

describe Hamster::Set do

  [:add, :<<].each do |method|

    describe "##{method}" do

      before do
        @original = Hamster.set("A", "B", "C")
      end

      describe "with a unique value" do

        before do
          @result = @original.send(method, "D")
        end

        it "preserves the original" do
          @original.should == Hamster.set("A", "B", "C")
        end

        it "returns a copy with the superset of values" do
          @result.should == Hamster.set("A", "B", "C", "D")
        end

      end

      describe "with a duplicate value" do

        before do
          @result = @original.send(method, "C")
        end

        it "preserves the original values" do
          @original.should == Hamster.set("A", "B", "C")
        end

        it "returns self" do
          @result.should equal(@original)
        end

      end

    end

  end

end
