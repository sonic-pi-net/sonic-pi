require "spec_helper"

require "hamster/hash"

describe Hamster::Hash do

  describe "#except" do

    before do
      @hash = Hamster.hash("A" => "aye", "B" => "bee", "C" => "see", nil => "NIL")
    end

    describe "with only keys that the Hash has" do

      it "returns a Hash without those values" do
        @hash.except("B", nil).should == Hamster.hash("A" => "aye", "C" => "see")
      end

    end

    describe "with keys that the Hash doesn't have" do

      it "returns a Hash without the values that it had keys for" do
        @hash.except("B", "A", 3).should == Hamster.hash("C" => "see", nil => "NIL")
      end

    end

  end

end
