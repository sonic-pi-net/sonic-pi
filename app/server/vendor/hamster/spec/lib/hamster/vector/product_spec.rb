require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do
  describe "#product" do
    context "when passed no arguments" do
      it "multiplies all items in vector" do
        [
          [[], 1],
          [[2], 2],
          [[1, 3, 5, 7, 11], 1155],
        ].each do |values, expected|
          Hamster.vector(*values).product.should == expected
        end
      end
    end

    context "when passed one or more vectors" do
      let(:vector) { Hamster.vector(1,2,3) }

      context "when passed a block" do
        it "yields an array for each combination of items from the vectors" do
          yielded = []
          vector.product(vector) { |obj| yielded << obj }
          yielded.should eql([[1,1], [1,2], [1,3], [2,1], [2,2], [2,3], [3,1], [3,2], [3,3]])

          yielded = []
          vector.product(Hamster.vector(3,4,5), Hamster.vector(6,8)) { |obj| yielded << obj }
          yielded.should eql(
            [[1, 3, 6], [1, 3, 8], [1, 4, 6], [1, 4, 8], [1, 5, 6], [1, 5, 8],
             [2, 3, 6], [2, 3, 8], [2, 4, 6], [2, 4, 8], [2, 5, 6], [2, 5, 8],
             [3, 3, 6], [3, 3, 8], [3, 4, 6], [3, 4, 8], [3, 5, 6], [3, 5, 8]])
        end

        it "returns self" do
          vector.product(Hamster.vector) {}.should be(vector)
          vector.product(Hamster.vector(1,2), Hamster.vector(3)) {}.should be(vector)
          Hamster.vector.product(vector) {}.should be(Hamster.vector)
        end
      end

      context "when not passed a block" do
        it "returns the cartesian product in an array" do
          Hamster.vector(1,2).product(Hamster.vector(3,4,5), Hamster.vector(6,8)).should eql(
            [[1, 3, 6], [1, 3, 8], [1, 4, 6], [1, 4, 8], [1, 5, 6], [1, 5, 8],
            [2, 3, 6], [2, 3, 8], [2, 4, 6], [2, 4, 8], [2, 5, 6], [2, 5, 8]])
        end
      end

      context "when one of the arguments is empty" do
        it "returns an empty array" do
          vector.product(Hamster.vector, Hamster.vector(4,5,6)).should eql([])
        end
      end

      context "when the receiver is empty" do
        it "returns an empty array" do
          Hamster.vector.product(vector, Hamster.vector(4,5,6)).should eql([])
        end
      end
    end

    context "when passed one or more Arrays" do
      it "also calculates the cartesian product correctly" do
        Hamster.vector(1,2).product([3,4,5], [6,8]).should eql(
          [[1, 3, 6], [1, 3, 8], [1, 4, 6], [1, 4, 8], [1, 5, 6], [1, 5, 8],
          [2, 3, 6], [2, 3, 8], [2, 4, 6], [2, 4, 8], [2, 5, 6], [2, 5, 8]])
      end
    end
  end
end