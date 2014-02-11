require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do

  [:each, :foreach].each do |method|

    describe "##{method}" do

      describe "with no block" do

        before do
          @vector = Hamster.vector("A", "B", "C")
          @result = @vector.send(method)
        end

        it "returns self" do
          @result.should equal(@vector)
        end

      end

      describe "with a block" do

        before do
          @vector = Hamster.vector(*(1..1025))
          @items = []
          @result = @vector.send(method) { |item| @items << item }
        end

        it "returns nil" do
          @result.should be_nil
        end

        it "iterates over the items in order" do
          @items.should == (1..1025).to_a
        end

      end

    end

  end

end
