require "spec_helper"
require "hamster/set"

describe Hamster::Set do
  describe "#hash" do
    describe "on an empty set" do
      before do
        @result = Hamster.set.hash
      end

      it "returns 0" do
        @result.should == 0
      end
    end

    it "values are sufficiently distributed" do
      (1..4000).each_slice(4).map { |a, b, c, d| Hamster.set(a, b, c, d).hash }.uniq.size.should == 1000
    end
  end
end
