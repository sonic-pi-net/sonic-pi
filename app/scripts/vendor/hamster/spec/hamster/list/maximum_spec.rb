require "spec_helper"

require "hamster/list"

describe Hamster::List do

  [:maximum, :max].each do |method|

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
          [[], nil],
          [["A"], "A"],
          [%w[Ichi Ni San], "Ichi"],
        ].each do |values, expected|

          describe "on #{values.inspect}" do

            before do
              original = Hamster.list(*values)
              @result = original.send(method) { |maximum, item| item.length <=> maximum.length }
            end

            it "returns #{expected.inspect}" do
              @result.should == expected
            end

          end

        end

      end

      describe "without a block" do

        [
          [[], nil],
          [["A"], "A"],
          [%w[Ichi Ni San], "San"],
        ].each do |values, expected|

          describe "on #{values.inspect}" do

            before do
              original = Hamster.list(*values)
              @result = original.send(method)
            end

            it "returns #{expected.inspect}" do
              @result.should == expected
            end

          end

        end

      end

    end

  end

end
