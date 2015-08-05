require "spec_helper"
require "hamster/hash"

describe Hamster::Hash do
  describe "#update_in" do
    let(:hash) {
      Hamster::Hash[
        "A" => "aye",
        "B" => Hamster::Hash["C" => "see", "D" => Hamster::Hash["E" => "eee"]],
        "F" => Hamster::Vector["G", Hamster::Hash["H" => "eitch"], "I"]
      ]
    }

    context "with one level on existing key" do
      it "passes the value to the block" do
        hash.update_in("A") { |value| value.should == "aye" }
      end

      it "replaces the value with the result of the block" do
        result = hash.update_in("A") { |value| "FLIBBLE" }
        result.get("A").should == "FLIBBLE"
      end

      it "should preserve the original" do
        result = hash.update_in("A") { |value| "FLIBBLE" }
        hash.get("A").should == "aye"
      end
    end

    context "with multi-level on existing keys" do
      it "passes the value to the block" do
        hash.update_in("B", "D", "E") { |value| value.should == "eee" }
      end

      it "replaces the value with the result of the block" do
        result = hash.update_in("B", "D", "E") { |value| "FLIBBLE" }
        result["B"]["D"]["E"].should == "FLIBBLE"
      end

      it "should preserve the original" do
        result = hash.update_in("B", "D", "E") { |value| "FLIBBLE" }
        hash["B"]["D"]["E"].should == "eee"
      end
    end

    context "with multi-level creating sub-hashes when keys don't exist" do
      it "passes nil to the block" do
        hash.update_in("B", "X", "Y") { |value| value.should be_nil }
      end

      it "creates subhashes on the way to set the value" do
        result = hash.update_in("B", "X", "Y") { |value| "NEWVALUE" }
        result["B"]["X"]["Y"].should == "NEWVALUE"
        result["B"]["D"]["E"].should == "eee"
      end
    end

    context "with multi-level including vector with existing keys" do
      it "passes the value to the block" do
        hash.update_in("F", 1, "H") { |value| value.should == "eitch" }
      end

      it "replaces the value with the result of the block" do
        result = hash.update_in("F", 1, "H") { |value| "FLIBBLE" }
        result["F"][1]["H"].should == "FLIBBLE"
      end

      it "should preserve the original" do
        result = hash.update_in("F", 1, "H") { |value| "FLIBBLE" }
        hash["F"][1]["H"].should == "eitch"
      end
    end

    context "with empty key_path" do
      it "raises ArguemntError" do
        expect { hash.update_in() { |v| 42 } }.to raise_error(ArgumentError)
      end
    end
  end
end
