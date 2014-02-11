require "spec_helper"

require "hamster/experimental/mutable_set"

describe Hamster::MutableSet do

  [:add, :<<].each do |method|

    describe "##{method}" do

      before do
        @set = Hamster.mutable_set("A", "B", "C")
      end

      describe "with a unique value" do

        before do
          @result = @set.send(method, "D")
        end

        it "returns self" do
          @result.should equal(@set)
        end

        it "modifies the set to include the new value" do
          @set.should == Hamster.mutable_set("A", "B", "C", "D")
        end

      end

      describe "with a duplicate value" do

        before do
          @result = @set.send(method, "C")
        end

        it "preserves the original values" do
          @set.should == Hamster.mutable_set("A", "B", "C")
        end

        it "returns self" do
          @result.should equal(@set)
        end

      end

    end

  end

end
