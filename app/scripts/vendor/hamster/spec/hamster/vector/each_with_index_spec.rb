require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do

  describe "#each_with_index" do

    describe "with no block" do

      before do
        @vector = Hamster.vector("A", "B", "C")
        @result = @vector.each_with_index
      end

      it "returns self" do
        @result.should equal(@vector)
      end

    end

    describe "with a block" do

      before do
        @vector = Hamster.vector(*(1..1025))
        @pairs = []
        @result = @vector.each_with_index { |item, index| @pairs << [item, index] }
      end

      it "returns nil" do
        @result.should be_nil
      end

      it "iterates over the items in order" do
        @pairs.should == (1..@vector.size).zip(0..@vector.size.pred)
      end

    end

  end

end
