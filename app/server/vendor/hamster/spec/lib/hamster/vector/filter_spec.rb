require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do
  [:filter, :select, :find_all].each do |method|
    describe "##{method}" do
      let(:vector) { Hamster.vector("A", "B", "C") }

      describe "with a block" do
        it "preserves the original" do
          vector.send(method) { |item| item == "A" }
          vector.should eql(Hamster.vector("A", "B", "C"))
        end

        it "returns a vector with the matching values" do
          vector.send(method) { |item| item == "A" }.should eql(Hamster.vector("A"))
        end
      end

      describe "with no block" do
        it "returns an Enumerator" do
          vector.send(method).class.should be(Enumerator)
          vector.send(method).each { |item| item == "A" }.should eql(Hamster.vector("A"))
        end
      end

      describe "when nothing matches" do
        it "preserves the original" do
          vector.send(method) { |item| false }
          vector.should eql(Hamster.vector("A", "B", "C"))
        end

        it "returns an empty vector" do
          vector.send(method) { |item| false }.should equal(Hamster.vector)
        end
      end

      context "on an empty vector" do
        it "returns self" do
          Hamster.vector.send(method) { |item| true }.should be(Hamster.vector)
        end
      end

      context "from a subclass" do
        it "returns an instance of the subclass" do
          subclass = Class.new(Hamster::Vector)
          instance = subclass[1,2,3]
          instance.send(method) { |x| x > 1 }.class.should be(subclass)
        end
      end

      it "works with a variety of inputs" do
        [1, 2, 10, 31, 32, 33, 1023, 1024, 1025].each do |size|
          [0, 5, 32, 50, 500, 800, 1024].each do |threshold|
            vector = V.new(1..size)
            result = vector.send(method) { |item| item <= threshold }
            result.size.should == [size, threshold].min
            result.should eql(V.new(1..[size, threshold].min))
          end
        end
      end
    end
  end
end