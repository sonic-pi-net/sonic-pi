require "spec_helper"

require "hamster/list"

describe Hamster::List do

  describe "#join" do

    describe "on a really big list" do

      before do
        @list = Hamster.interval(0, STACK_OVERFLOW_DEPTH)
      end

      it "doesn't run out of stack" do
        -> { @list.join }.should_not raise_error
      end

    end

    describe "with a separator" do

      [
        [[], ""],
        [["A"], "A"],
        [%w[A B C], "A|B|C"]
      ].each do |values, expected|

        describe "on #{values.inspect}" do

          before do
            @original = Hamster.list(*values)
            @result = @original.join("|")
          end

          it "preserves the original" do
            @original.should == Hamster.list(*values)
          end

          it "returns #{expected.inspect}" do
            @result.should == expected
          end

        end

      end

    end

    describe "without a separator" do

      [
        [[], ""],
        [["A"], "A"],
        [%w[A B C], "ABC"]
      ].each do |values, expected|

        describe "on #{values.inspect}" do

          before do
            @original = Hamster.list(*values)
            @result = @original.join
          end

          it "preserves the original" do
            @original.should == Hamster.list(*values)
          end

          it "returns #{expected.inspect}" do
            @result.should == expected
          end

        end

      end

    end

  end

end
