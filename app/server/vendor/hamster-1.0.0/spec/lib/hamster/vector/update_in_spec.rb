require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do
  describe "#update_in" do
    let(:vector) {
      Hamster::Vector[
        100,
        101,
        102,
        Hamster::Vector[200, 201, Hamster::Vector[300, 301, 302]],
        Hamster::Hash["A" => "alpha", "B" => "bravo"],
        [400, 401, 402]
      ]
    }

    context "with one level on existing key" do
      it "passes the value to the block" do
        vector.update_in(1) { |value| value.should == 101 }
      end

      it "replaces the value with the result of the block" do
        result = vector.update_in(1) { |value| "FLIBBLE" }
        result.get(1).should == "FLIBBLE"
      end

      it "should preserve the original" do
        result = vector.update_in(1) { |value| "FLIBBLE" }
        vector.get(1).should == 101
      end
    end

    context "with multi-level vectors on existing keys" do
      it "passes the value to the block" do
        vector.update_in(3, 2, 0) { |value| value.should == 300 }
      end

      it "replaces the value with the result of the block" do
        result = vector.update_in(3, 2, 0) { |value| "FLIBBLE" }
        result[3][2][0].should == "FLIBBLE"
      end

      it "should preserve the original" do
        result = vector.update_in(3, 2, 0) { |value| "FLIBBLE" }
        vector[3][2][0].should == 300
      end
    end

    context "with multi-level creating sub-hashes when keys don't exist" do
      it "passes nil to the block" do
        vector.update_in(3, 3, "X", "Y") { |value| value.should be_nil }
      end

      it "creates subhashes on the way to set the value" do
        result = vector.update_in(3, 3, "X", "Y") { |value| "NEWVALUE" }
        result[3][3]["X"]["Y"].should == "NEWVALUE"
        result[3][2][0].should == 300
      end
    end

    context "with multi-level including hash with existing keys" do
      it "passes the value to the block" do
        vector.update_in(4, "B") { |value| value.should == "bravo" }
      end

      it "replaces the value with the result of the block" do
        result = vector.update_in(4, "B") { |value| "FLIBBLE" }
        result[4]["B"].should == "FLIBBLE"
      end

      it "should preserve the original" do
        result = vector.update_in(4, "B") { |value| "FLIBBLE" }
        vector[4]["B"].should == "bravo"
      end
    end

    context "with empty key_path" do
      it "raises ArguemntError" do
        expect { vector.update_in() { |v| 42 } }.to raise_error(ArgumentError)
      end
    end
  end
end
