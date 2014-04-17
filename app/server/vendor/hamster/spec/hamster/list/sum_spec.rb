require "spec_helper"

require "hamster/list"

describe Hamster::List do

  describe "#sum" do

    describe "on a really big list" do

      before do
        @list = Hamster.interval(0, STACK_OVERFLOW_DEPTH)
      end

      it "doesn't run out of stack" do
        -> { @list.sum }.should_not raise_error
      end

    end

    [
      [[], 0],
      [[2], 2],
      [[1, 3, 5, 7, 11], 27],
    ].each do |values, expected|

      describe "on #{values.inspect}" do

        before do
          original = Hamster.list(*values)
          @result = original.sum
        end

        it "returns #{expected.inspect}" do
          @result.should == expected
        end

      end

    end

  end

end
