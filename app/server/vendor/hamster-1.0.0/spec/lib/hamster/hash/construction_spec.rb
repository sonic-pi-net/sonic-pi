require "spec_helper"
require "hamster/hash"

describe Hamster::Hash do
  describe ".hash" do
    context "with nothing" do
      it "returns the canonical empty hash" do
        Hamster.hash.should be_empty
        Hamster.hash.should equal(Hamster::EmptyHash)
      end
    end

    context "with an implicit hash" do
      let(:hash) { Hamster.hash("A" => "aye", "B" => "bee", "C" => "see") }

      it "is equivalent to repeatedly using #put" do
        hash.should eql(Hamster.hash.put("A", "aye").put("B", "bee").put("C", "see"))
        hash.size.should == 3
      end
    end

    context "with an array of pairs" do
      let(:hash) { Hamster.hash([[:a, 1], [:b, 2]]) }

      it "initializes a new Hash" do
        hash.should eql(Hamster.hash(a: 1, b: 2))
      end
    end

    context "with a Hamster::Hash" do
      let(:hash) { Hamster.hash(a: 1, b: 2) }
      let(:other) { Hamster.hash(hash) }

      it "initializes an equivalent Hash" do
        hash.should eql(other)
      end
    end
  end
end