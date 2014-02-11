require "spec_helper"

require "hamster/tuple"

describe Hamster::Tuple do

  describe "#last" do

    before do
      @tuple = Hamster::Tuple.new("A", "B")
    end

    it "returns the last value" do
      @tuple.last.should == "B"
    end

  end

end
