require "spec_helper"
require "hamster/list"

describe Hamster::List do
  [:group_by, :group].each do |method|
    describe "##{method}" do
      context "on a really big list" do
        it "doesn't run out of stack" do
          -> { Hamster.interval(0, STACK_OVERFLOW_DEPTH).send(method) }.should_not raise_error
        end
      end

      context "with a block" do
        [
          [[], []],
          [[1], [true => Hamster.list(1)]],
          [[1, 2, 3, 4], [true => Hamster.list(3, 1), false => Hamster.list(4, 2)]],
        ].each do |values, expected|
          context "on #{values.inspect}" do
            it "returns #{expected.inspect}" do
              Hamster.list(*values).send(method, &:odd?).should eql(Hamster.hash(*expected))
            end
          end
        end
      end

      context "without a block" do
        [
          [[], []],
          [[1], [1 => Hamster.list(1)]],
          [[1, 2, 3, 4], [1 => Hamster.list(1), 2 => Hamster.list(2), 3 => Hamster.list(3), 4 => Hamster.list(4)]],
        ].each do |values, expected|
          context "on #{values.inspect}" do
            it "returns #{expected.inspect}" do
              Hamster.list(*values).send(method).should eql(Hamster.hash(*expected))
            end
          end
        end
      end
    end
  end
end