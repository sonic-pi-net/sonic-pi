require "spec_helper"

require "hamster/set"

describe Hamster::Set do

  describe "#one?" do

    describe "when empty" do

      before do
        @set = Hamster.set
      end

      it "with a block returns false" do
        @set.one? {}.should == false
      end

      it "with no block returns false" do
        @set.one?.should == false
      end

    end

    describe "when not empty" do

      describe "with a block" do

        before do
          @set = Hamster.set("A", "B", "C")
        end

        it "returns false if the block returns true more than once" do
          @set.one? { |item| true }.should == false
        end

        it "returns false if the block never returns true" do
          @set.one? { |item| false }.should == false
        end

        it "returns true if the block only returns true once" do
          @set.one? { |item| item == "A" }.should == true
        end

      end

      describe "with no block" do

        it "returns false if more than one value is truthy" do
          Hamster.set(nil, true, "A").one?.should == false
        end

        it "returns true if only one value is truthy" do
          Hamster.set(nil, true, false).one?.should == true
        end

      end

    end

  end

end
