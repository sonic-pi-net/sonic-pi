require "spec_helper"

require "hamster/list"

describe Hamster::List do

  describe "#tail" do

    describe "on a really big list" do

      before do
        @list = Hamster.interval(0, STACK_OVERFLOW_DEPTH)
      end

      it "doesn't run out of stack" do
        -> { @list.filter(&:nil?).tail }.should_not raise_error
      end

    end

    [
      [[], []],
      [["A"], []],
      [%w[A B C], %w[B C]],
    ].each do |values, expected|

      describe "on #{values.inspect}" do

        before do
          @original = Hamster.list(*values)
          @result = @original.tail
        end

        it "preserves the original" do
          @original.should == Hamster.list(*values)
        end

        it "returns #{expected.inspect}" do
          @result.should == Hamster.list(*expected)
        end

      end

    end

  end

end
