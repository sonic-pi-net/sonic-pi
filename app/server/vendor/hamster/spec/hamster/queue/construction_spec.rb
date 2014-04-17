require "spec_helper"

require "hamster/queue"

describe Hamster do

  describe ".queue" do

    describe "with no arguments" do

      before do
        @queue = Hamster.queue
      end

      it "always returns the same instance" do
        @queue.should equal(Hamster.queue)
      end

      it "returns an empty queue" do
        @queue.should be_empty
      end

    end

    describe "with a number of items" do

      before do
        @queue = Hamster.queue("A", "B", "C")
      end

      it "always returns a different instance" do
        @queue.should_not equal(Hamster.queue("A", "B", "C"))
      end

      it "is the same as repeatedly using #enqueue" do
        @queue.should == Hamster.queue.enqueue("A").enqueue("B").enqueue("C")
      end

    end

  end

end
