require "spec_helper"

require "hamster/list"

describe Hamster::List do

  describe "#one?" do

    describe "on a really big list" do

      before do
        @list = Hamster.interval(0, STACK_OVERFLOW_DEPTH)
      end

      it "doesn't run out of stack" do
        -> { @list.one? { false } }.should_not raise_error
      end

    end

    describe "when empty" do

      before do
        @list = Hamster.list
      end

      it "with a block returns false" do
        @list.one? {}.should == false
      end

      it "with no block returns false" do
        @list.one?.should == false
      end

    end

    describe "when not empty" do

      describe "with a block" do

        before do
          @list = Hamster.list("A", "B", "C")
        end

        it "returns false if the block returns true more than once" do
          @list.one? { |item| true }.should == false
        end

        it "returns false if the block never returns true" do
          @list.one? { |item| false }.should == false
        end

        it "returns true if the block only returns true once" do
          @list.one? { |item| item == "A" }.should == true
        end

      end

      describe "with no block" do

        it "returns false if more than one value is truthy" do
          Hamster.list(nil, true, "A").one?.should == false
        end

        it "returns true if only one value is truthy" do
          Hamster.list(nil, true, false).one?.should == true
        end

      end

    end

  end

end
