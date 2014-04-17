require "spec_helper"

require "hamster/list"

describe Hamster::List do

  [:at, :[]].each do |method|

    describe "##{method}" do

      describe "on a really big list" do

        before do
          @list = Hamster.interval(0, STACK_OVERFLOW_DEPTH)
        end

        it "doesn't run out of stack" do
          -> { @list.send(method, STACK_OVERFLOW_DEPTH) }.should_not raise_error
        end

      end

      [
        [[], 10, nil],
        [["A"], 10, nil],
        [%w[A B C], 0, "A"],
        [%w[A B C], 2, "C"],
      ].each do |values, number, expected|

        describe "#{values.inspect} with #{number}" do

          before do
            @original = Hamster.list(*values)
            @result = @original.send(method, number)
          end

          it "returns #{expected.inspect}" do
            @result.should == expected
          end

        end

      end

    end

  end

end
