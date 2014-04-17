require "spec_helper"

require "hamster/list"

describe Hamster::List do

  [:each_chunk, :each_slice].each do |method|

    describe "##{method}" do

      describe "on a really big list" do

        before do
          @list = Hamster.interval(0, STACK_OVERFLOW_DEPTH)
        end

        it "doesn't run out of stack" do
          -> { @list.send(method, 1) { |item| } }.should_not raise_error
        end

      end

      [
        [[], []],
        [["A"], [Hamster.list("A")]],
        [%w[A B C], [Hamster.list("A", "B"), Hamster.list("C")]],
      ].each do |values, expected|

        describe "on #{values.inspect}" do

          before do
            @original = Hamster.list(*values)
          end

          describe "with a block" do

            before do
              @items = []
              @result = @original.send(method, 2) { |item| @items << item }
            end

            it "preserves the original" do
              @original.should == Hamster.list(*values)
            end

            it "iterates over the items in order" do
              @items.should == expected
            end

            it "returns nil" do
              @result.should be_nil
            end

          end

          describe "without a block" do

            before do
              @result = @original.send(method, 2)
            end

            it "preserves the original" do
              @original.should == Hamster.list(*values)
            end

            it "returns the expected items" do
              @result.should == Hamster.list(*expected)
            end

          end

        end

      end

    end

  end

end
