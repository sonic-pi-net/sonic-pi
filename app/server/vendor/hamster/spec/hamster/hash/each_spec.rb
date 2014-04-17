require "spec_helper"

require "hamster/hash"

describe Hamster::Hash do

  [:each, :foreach].each do |method|

    describe "##{method}" do

      before do
        @hash = Hamster.hash("A" => "aye", "B" => "bee", "C" => "see")
      end

      describe "with a block (internal iteration)" do

        it "returns nil" do
          @hash.send(method) {}.should be_nil
        end

        it "yields all key/value pairs" do
          actual_pairs = {}
          @hash.send(method) { |key, value| actual_pairs[key] = value }
          actual_pairs.should == { "A" => "aye", "B" => "bee", "C" => "see" }
        end

      end

      describe "with no block" do

        it "returns self" do
          @hash.send(method).should equal(@hash)
        end

      end

    end

  end

end
