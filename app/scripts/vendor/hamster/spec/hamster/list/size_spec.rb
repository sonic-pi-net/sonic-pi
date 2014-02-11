require "spec_helper"

require "hamster/list"

describe Hamster::List do

  [:size, :length].each do |method|

    describe "##{method}" do

      describe "on a really big list" do

        before do
          @list = Hamster.interval(0, STACK_OVERFLOW_DEPTH)
        end

        it "doesn't run out of stack" do
          -> { @list.size }.should_not raise_error
        end

      end

      [
        [[], 0],
        [["A"], 1],
        [%w[A B C], 3],
      ].each do |values, expected|

        describe "on #{values.inspect}" do

          before do
            @list = Hamster.list(*values)
          end

          it "returns #{expected.inspect}" do
            @list.send(method).should == expected
          end

        end

      end

    end

  end

end
