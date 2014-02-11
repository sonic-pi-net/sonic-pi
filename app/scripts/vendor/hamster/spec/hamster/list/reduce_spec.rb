require "spec_helper"

require "hamster/list"

describe Hamster::List do
  [:reduce, :inject, :fold, :foldr].each do |method|
    describe "##{method}" do
      describe "on a really big list" do
        before do
          @list = Hamster.interval(0, STACK_OVERFLOW_DEPTH)
        end

        it "doesn't run out of stack" do
          -> { @list.reduce(&:+) }.should_not raise_error
        end
      end

      [
        [[], 10, 10],
        [[1], 10, 9],
        [[1, 2, 3], 10, 4],
      ].each do |values, initial, expected|

        describe "on #{values.inspect}" do
          before do
            @list = Hamster.list(*values)
          end

          describe "with an initial value of #{initial}" do
            describe "and a block" do
              it "returns #{expected.inspect}" do
                @list.send(method, initial) { |memo, item| memo - item }.should == expected
              end
            end

            describe "and no block" do
              it "returns the memo" do
                @list.send(method, initial).should == initial
              end
            end
          end
        end
      end

      [
        [[], nil],
        [[1], 1],
        [[1, 2, 3], -4],
      ].each do |values, expected|

        describe "on #{values.inspect}" do
          before do
            @list = Hamster.list(*values)
          end

          describe "with no initial value" do
            describe "and a block" do
              it "returns #{expected.inspect}" do
                @list.send(method) { |memo, item| memo - item }.should == expected
              end
            end

            describe "and no block" do
              it "returns nil" do
                @list.send(method).should be_nil
              end
            end
          end
        end
      end
    end
  end
end
