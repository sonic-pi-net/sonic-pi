require "spec_helper"
require "hamster/hash"

describe Hamster::Hash do
  describe "#values_at" do
    it "returns a vector of values for the given keys" do
      h = Hamster.hash(:a => 9, :b => 'a', :c => -10, :d => nil)
      h.values_at.should be_kind_of(Hamster::Vector)
      h.values_at.should eql(Hamster.vector)
      h.values_at(:a, :d, :b).should be_kind_of(Hamster::Vector)
      h.values_at(:a, :d, :b).should eql(Hamster::Vector[9, nil, 'a'])
    end
  end
end