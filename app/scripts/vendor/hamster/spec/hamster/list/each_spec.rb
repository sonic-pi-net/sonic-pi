require "spec_helper"

require "hamster/list"

describe Hamster::List do

  [:each, :foreach].each do |method|

    describe "##{method}" do

      describe "on a really big list" do

        before do
          @list = Hamster.interval(0, STACK_OVERFLOW_DEPTH)
        end

        it "doesn't run out of stack" do
          -> { @list.send(method) { |item| } }.should_not raise_error
        end

      end

      [
        [],
        ["A"],
        %w[A B C],
      ].each do |values|

        describe "on #{values.inspect}" do

          before do
            @original = Hamster.list(*values)
          end

          describe "with a block" do

            before do
              @items = []
              @result = @original.send(method) { |item| @items << item }
            end

            it "iterates over the items in order" do
              @items.should == values
            end

            it "returns nil" do
              @result.should be_nil
            end

          end

          describe "without a block" do

            before do
              @result = @original.send(method)
            end

            it "returns self" do
              @result.should equal(@original)
            end

          end

        end

      end

    end

  end

end
