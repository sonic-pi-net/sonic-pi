require "spec_helper"

require "hamster/hash"

describe Hamster::Hash do

  [:all?, :forall?].each do |method|

    describe "##{method}" do

      describe "when empty" do

        before do
          @hash = Hamster.hash
        end

        it "with a block returns true" do
          @hash.send(method) {}.should == true
        end

        it "with no block returns true" do
          @hash.send(method).should == true
        end

      end

      describe "when not empty" do

        before do
          @hash = Hamster.hash("A" => "aye", "B" => "bee", "C" => "see")
        end

        describe "with a block" do

          it "returns true if the block always returns true" do
            @hash.send(method) { |key, value| true }.should == true
          end

          it "returns false if the block ever returns false" do
            @hash.send(method) { |key, value| key == "D" || value == "dee" }.should == false
          end

        end

        describe "with no block" do

          it "returns true" do
            @hash.send(method).should == true
          end

        end

      end

    end

  end

end
