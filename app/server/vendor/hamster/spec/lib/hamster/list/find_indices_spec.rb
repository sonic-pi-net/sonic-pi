require "spec_helper"
require "hamster/list"

describe Hamster::List do
  [:find_indices, :indices].each do |method|
    describe "##{method}" do
      it "is lazy" do
        count = 0
        Hamster.stream { count += 1 }.send(method) { |item| true }
        count.should <= 1
      end

      context "on a large list which doesn't contain desired item" do
        it "doesn't blow the stack" do
          -> { Hamster.interval(0, STACK_OVERFLOW_DEPTH).find_indices { |x| x < 0 }.size }.should_not raise_error
        end
      end

      [
        [[], "A", []],
        [["A"], "B", []],
        [%w[A B A], "B", [1]],
        [%w[A B A], "A", [0, 2]],
        [[2], 2, [0]],
        [[2], 2.0, [0]],
        [[2.0], 2.0, [0]],
        [[2.0], 2, [0]],
      ].each do |values, item, expected|
        context "looking for #{item.inspect} in #{values.inspect}" do
          it "returns #{expected.inspect}" do
            Hamster.list(*values).send(method) { |x| x == item }.should eql(Hamster.list(*expected))
          end
        end
      end
    end
  end
end