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

    it "generates the same hash value for the a hash regardless of the order things were added to it" do
      # The keys :issued_at and :expires_at happen to end up in the same
      # bucket in the trie, so depending on which one gets added first, one
      # of them will end up in a child trie. The hash value should be the
      # same regardless of which is added first
      Hamster.hash.put(:issued_at, nil).put(:expires_at, nil).hash.should == Hamster.hash.put(:expires_at, nil).put(:issued_at, nil).hash
    end

    describe "on an empty hash" do
      before do
        @result = Hamster.hash.hash
      end

      it "returns 0" do
        @result.should == 0
      end
    end

    describe "from a subclass" do
      before do
        @subclass = Class.new(Hamster::Hash)
        @instance = @subclass.new("some" => "values")
      end

      it "should return an instance of the subclass" do
        @instance.class.should be @subclass
      end

      it "should return a frozen instance" do
        @instance.frozen?.should be true
      end
    end
  end
end
