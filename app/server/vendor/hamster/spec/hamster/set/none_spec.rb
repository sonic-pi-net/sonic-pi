require "spec_helper"

require "hamster/set"

describe Hamster::Set do

  describe "#none?" do

    describe "when empty" do

      before do
        @set = Hamster.set
      end

      it "with a block returns true" do
        @set.none? {}.should == true
      end

      it "with no block returns true" do
        @set.none?.should == true
      end

    end

    describe "when not empty" do

      describe "with a block" do

        before do
          @set = Hamster.set("A", "B", "C", nil)
        end

        ["A", "B", "C", nil].each do |value|

          it "returns false if the block ever returns true (#{value.inspect})" do
            @set.none? { |item| item == value }.should == false
          end

        end

        it "returns true if the block always returns false" do
          @set.none? { |item| item == "D" }.should == true
        end

      end

      describe "with no block" do

        it "returns false if any value is truthy" do
          Hamster.set(nil, false, true, "A").none?.should == false
        end

        it "returns true if all values are falsey" do
          Hamster.set(nil, false).none?.should == true
        end

      end

    end

  end

end
