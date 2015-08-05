require "spec_helper"
require "hamster/deque"

describe Hamster::Deque do
  describe ".deque" do
    context "with no arguments" do
      it "always returns the same instance" do
        Hamster.deque.class.should be(Hamster::Deque)
        Hamster.deque.should equal(Hamster.deque)
      end

      it "returns an empty, frozen deque" do
        Hamster.deque.should be_empty
        Hamster.deque.should be_frozen
      end
    end

    context "with a number of items" do
      let(:deque) { Hamster.deque("A", "B", "C") }

      it "always returns a different instance" do
        deque.should_not equal(Hamster.deque("A", "B", "C"))
      end

      it "is the same as repeatedly using #endeque" do
        deque.should eql(Hamster.deque.enqueue("A").enqueue("B").enqueue("C"))
      end
    end
  end
end