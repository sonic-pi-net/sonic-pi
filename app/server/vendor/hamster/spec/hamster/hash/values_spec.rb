require "spec_helper"

require "hamster/hash"
require "hamster/set"

describe Hamster::Hash do

  describe "#values" do

    before do
      hash = Hamster.hash("A" => "aye", "B" => "bee", "C" => "see")
      @result = hash.values
    end

    it "returns the keys as a list" do
      @result.should be_a Hamster::List
      @result.to_a.sort.should == %w(aye bee see)
    end

  end

  describe "#values with duplicates" do
    before do
      hash = Hamster.hash(:A => 15, :B => 19, :C => 15)
      @result = hash.values
    end

    it "returns the keys as a list" do
      @result.should be_a Hamster::List
      @result.to_a.sort.should == [15, 15, 19]
    end
  end

end
