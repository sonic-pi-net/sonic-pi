require "spec_helper"

require "hamster/list"

describe Hamster::List do

  describe "#last" do

    describe "on a really big list" do

      before do
        @list = Hamster.interval(0, STACK_OVERFLOW_DEPTH)
      end

      it "doesn't run out of stack" do
        -> { @list.last }.should_not raise_error
      end

    end

    [
      [[], nil],
      [["A"], "A"],
      [%w[A B C], "C"],
    ].each do |values, expected|

      describe "on #{values.inspect}" do

        before do
          original = Hamster.list(*values)
          @result = original.last
        end

        it "returns #{expected.inspect}" do
          @result.should == expected
        end

      end

    end

  end

end
