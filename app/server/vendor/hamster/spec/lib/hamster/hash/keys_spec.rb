require "spec_helper"
require "hamster/hash"
require "hamster/set"

describe Hamster::Hash do
  describe "#keys" do
    let(:hash) { Hamster.hash("A" => "aye", "B" => "bee", "C" => "see") }

    it "returns the keys as a set" do
      hash.keys.should eql(Hamster.set("A", "B", "C"))
    end

    it "returns frozen String keys" do
      hash.keys.each { |s| s.should be_frozen }
    end
  end
end