require "spec_helper"

require "hamster/list"

describe Hamster::List do

  [:include?, :member?, :contains?, :elem?].each do |method|

    describe "##{method}" do

      describe "on a really big list" do

        before do
          @list = Hamster.interval(0, STACK_OVERFLOW_DEPTH)
        end

        it "doesn't run out of stack" do
          -> { @list.send(method, nil) }.should_not raise_error
        end

      end

      [
        [[], "A", false],
        [[], nil, false],
        [["A"], "A", true],
        [["A"], "B", false],
        [["A"], nil, false],
        [["A", "B", nil], "A", true],
        [["A", "B", nil], "B", true],
        [["A", "B", nil], nil, true],
        [["A", "B", nil], "C", false],
        [[2], 2, true],
        [[2], 2.0, true],
        [[2.0], 2.0, true],
        [[2.0], 2, true],
      ].each do |values, item, expected|

        describe "on #{values.inspect}" do

          before do
            @list = Hamster.list(*values)
          end

          it "returns #{expected.inspect}" do
            @list.send(method, item).should == expected
          end

        end

      end

    end

  end

end
