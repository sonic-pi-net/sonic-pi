require "spec_helper"

require "hamster/list"

describe Hamster::List do

  [:find, :detect].each do |method|

    describe "##{method}" do

      describe "on a really big list" do

        before do
          @list = Hamster.interval(0, STACK_OVERFLOW_DEPTH)
        end

        it "doesn't run out of stack" do
          -> { @list.send(method) { false } }.should_not raise_error
        end

      end

      [
        [[], "A", nil],
        [[], nil, nil],
        [["A"], "A", "A"],
        [["A"], "B", nil],
        [["A"], nil, nil],
        [["A", "B", nil], "A", "A"],
        [["A", "B", nil], "B", "B"],
        [["A", "B", nil], nil, nil],
        [["A", "B", nil], "C", nil],
      ].each do |values, item, expected|

        describe "on #{values.inspect}" do

          before do
            @list = Hamster.list(*values)
          end

          describe "with a block" do

            it "returns #{expected.inspect}" do
              @list.send(method) { |x| x == item }.should == expected
            end

          end

          describe "without a block" do

            it "returns nil" do
              @list.send(method).should be_nil
            end

          end

        end

      end

    end

  end

end
