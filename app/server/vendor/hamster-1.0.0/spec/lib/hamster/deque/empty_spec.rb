require "spec_helper"
require "hamster/deque"

describe Hamster::Deque do
  describe "#empty?" do
    [
      [[], true],
      [["A"], false],
      [%w[A B C], false],
    ].each do |values, expected|
      context "on #{values.inspect}" do
        it "returns #{expected.inspect}" do
          Hamster.deque(*values).empty?.should == expected
        end
      end
    end

    context "after dedequeing an item from #{%w[A B C].inspect}" do
      it "returns false" do
        Hamster.deque("A", "B", "C").dequeue.should_not be_empty
      end
    end
  end

  describe ".empty" do
    it "returns the canonical empty vector" do
      Hamster::Deque.empty.size.should be(0)
      Hamster::Deque.empty.class.should be(Hamster::Deque)
      Hamster::Deque.empty.object_id.should be(Hamster::EmptyDeque.object_id)
    end

    context "from a subclass" do
      it "returns an empty instance of the subclass" do
        subclass = Class.new(Hamster::Deque)
        subclass.empty.class.should be(subclass)
        subclass.empty.should be_empty
      end
    end
  end
end