require "spec_helper"

require "hamster/stack"

describe Hamster do

  describe ".stack" do

    describe "with no arguments" do

      before do
        @stack = Hamster.stack
      end

      it "always returns the same instance" do
        @stack.should equal(Hamster.stack)
      end

      it "returns an empty stack" do
        @stack.should be_empty
      end

    end

    describe "with a number of items" do

      before do
        @stack = Hamster.stack("A", "B", "C")
      end

      it "always returns a different instance" do
        @stack.should_not equal(Hamster.stack("A", "B", "C"))
      end

      it "is the same as repeatedly using #push" do
        @stack.should == Hamster.stack.push("A").push("B").push("C")
      end

    end

  end

end
