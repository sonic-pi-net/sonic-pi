require "spec_helper"

require "hamster/hash"

describe Hamster::Hash do

  describe ".hash" do

    describe "with nothing" do

      before do
        @hash = Hamster.hash
      end

      it "returns an empty hash" do
        @hash.should be_empty
      end

    end

    describe "with an implicit hash" do

      before do
        @hash = Hamster.hash("A" => "aye", "B" => "bee", "C" => "see")
      end

      it "is equivalent to repeatedly using #put" do
        @hash.should == Hamster.hash.put("A", "aye").put("B", "bee").put("C", "see")
      end

    end

  end

end
