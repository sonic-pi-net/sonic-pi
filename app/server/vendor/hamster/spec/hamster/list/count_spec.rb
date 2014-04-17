require "spec_helper"

require "hamster/list"

describe Hamster::List do

  describe "#count" do

    describe "on a really big list" do

      before do
        @list = Hamster.interval(0, STACK_OVERFLOW_DEPTH)
      end

      it "doesn't run out of stack" do
        -> { @list.count }.should_not raise_error
      end

    end

    [
      [[], 0],
      [[1], 1],
      [[1, 2], 1],
      [[1, 2, 3], 2],
      [[1, 2, 3, 4], 2],
      [[1, 2, 3, 4, 5], 3],
    ].each do |values, expected|

      describe "on #{values.inspect}" do

        before do
          @original = Hamster.list(*values)
        end

        describe "with a block" do

          before do
            @result = @original.count(&:odd?)
          end

          it "returns #{expected.inspect}" do
            @result.should == expected
          end

        end

        describe "without a block" do

          before do
            @result = @original.count
          end

          it "returns length" do
            @result.should == @original.length
          end

        end

      end

    end

  end

end
