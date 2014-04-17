require "spec_helper"

require "hamster/list"

describe Hamster::List do

  [:group_by, :group].each do |method|

    describe "##{method}" do

      describe "on a really big list" do

        before do
          @list = Hamster.interval(0, STACK_OVERFLOW_DEPTH)
        end

        it "doesn't run out of stack" do
          -> { @list.send(method) }.should_not raise_error
        end

      end

      describe "with a block" do

        [
          [[], []],
          [[1], [true => Hamster.list(1)]],
          [[1, 2, 3, 4], [true => Hamster.list(3, 1), false => Hamster.list(4, 2)]],
        ].each do |values, expected|

          describe "on #{values.inspect}" do

            before do
              original = Hamster.list(*values)
              @result = original.send(method, &:odd?)
            end

            it "returns #{expected.inspect}" do
              @result.should == Hamster.hash(*expected)
            end

          end

        end

      end

      describe "without a block" do

        [
          [[], []],
          [[1], [1 => Hamster.list(1)]],
          [[1, 2, 3, 4], [1 => Hamster.list(1), 2 => Hamster.list(2), 3 => Hamster.list(3), 4 => Hamster.list(4)]],
        ].each do |values, expected|

          describe "on #{values.inspect}" do

            before do
              original = Hamster.list(*values)
              @result = original.send(method)
            end

            it "returns #{expected.inspect}" do
              @result.should == Hamster.hash(*expected)
            end

          end

        end

      end

    end

  end

end
