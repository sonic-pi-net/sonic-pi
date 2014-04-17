require "spec_helper"
require "hamster/list"

describe Hamster::List do
  describe "#hash" do
    describe "on a really big list" do
      before do
        @list = Hamster.interval(0, STACK_OVERFLOW_DEPTH)
      end

      it "doesn't run out of stack" do
        -> { @list.hash }.should_not raise_error
      end
    end

    describe "on an empty list" do
      before do
        @result = Hamster.list.hash
      end

      it "returns 0" do
        expect(@result).to eq(0)
      end
    end

    it "values are sufficiently distributed" do
      (1..4000).each_slice(4).map { |a, b, c, d| Hamster.list(a, b, c, d).hash }.uniq.size.should == 1000
    end
  end
end
