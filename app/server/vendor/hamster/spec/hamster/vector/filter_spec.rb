require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do

  [:filter, :select, :find_all].each do |method|

    describe "##{method}" do

      before do
        @original = Hamster.vector("A", "B", "C")
      end

      describe "with a block" do

        before do
          @result = @original.send(method) { |item| item == "A" }
        end

        it "preserves the original" do
          @original.should == Hamster.vector("A", "B", "C")
        end

        it "returns a vector with the matching values" do
          @result.should == Hamster.vector("A")
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

      describe "when nothing matches" do

        before do
          @result = @original.send(method) { |item| false }
        end

        it "preserves the original" do
          @original.should == Hamster.vector("A", "B", "C")
        end

        it "returns an empty vector" do
          @result.should equal(Hamster.vector)
        end

      end

    end

  end

end
