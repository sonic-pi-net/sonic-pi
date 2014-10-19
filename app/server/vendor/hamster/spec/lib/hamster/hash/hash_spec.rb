require "spec_helper"
require "hamster/hash"

describe Hamster::Hash do
  describe "#hash" do
    it "values are sufficiently distributed" do
      (1..4000).each_slice(4).map { |ka, va, kb, vb| Hamster.hash(ka => va, kb => vb).hash }.uniq.size.should == 1000
    end

    it "differs given the same keys and different values" do
      Hamster.hash("ka" => "va").hash.should_not == Hamster.hash("ka" => "vb").hash
    end

    it "differs given the same values and different keys" do
      Hamster.hash("ka" => "va").hash.should_not == Hamster.hash("kb" => "va").hash
    end

    it "generates the same hash value for a hash regardless of the order things were added to it" do
      key1 = DeterministicHash.new('abc', 1)
      key2 = DeterministicHash.new('xyz', 1)
      Hamster.hash.put(key1, nil).put(key2, nil).hash.should == Hamster.hash.put(key2, nil).put(key1, nil).hash
    end

    describe "on an empty hash" do
      it "returns 0" do
        Hamster.hash.hash.should == 0
      end
    end
  end
end