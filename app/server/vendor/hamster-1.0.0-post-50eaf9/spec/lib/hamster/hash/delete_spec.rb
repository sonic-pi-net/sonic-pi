require "spec_helper"
require "hamster/hash"

describe Hamster::Hash do
  describe "#delete" do
    let(:hash) { Hamster.hash("A" => "aye", "B" => "bee", "C" => "see") }

    context "with an existing key" do
      let(:result) { hash.delete("B") }

      it "preserves the original" do
        hash.should eql(Hamster.hash("A" => "aye", "B" => "bee", "C" => "see"))
      end

      it "returns a copy with the remaining key/value pairs" do
        result.should eql(Hamster.hash("A" => "aye", "C" => "see"))
      end
    end

    context "with a non-existing key" do
      let(:result) { hash.delete("D") }

      it "preserves the original values" do
        hash.should eql(Hamster.hash("A" => "aye", "B" => "bee", "C" => "see"))
      end

      it "returns self" do
        result.should equal(hash)
      end
    end

    context "when removing the last key" do
      context "from a Hash with no default block" do
        it "returns the canonical empty Hash" do
          hash.delete('A').delete('B').delete('C').should be(Hamster::EmptyHash)
        end
      end
    end
  end
end