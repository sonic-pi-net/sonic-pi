require "spec_helper"
require "hamster/list"

describe Hamster::List do
  [
    [:sort, ->(left, right) { left.length <=> right.length }],
    [:sort_by, ->(item) { item.length }],
  ].each do |method, comparator|
    describe "##{method}" do
      it "is lazy" do
        -> { Hamster.stream { fail }.send(method, &comparator) }.should_not raise_error
      end

      [
        [[], []],
        [["A"], ["A"]],
        [%w[Ichi Ni San], %w[Ni San Ichi]],
      ].each do |values, expected|
        context "on #{values.inspect}" do
          let(:list) { Hamster.list(*values) }

          context "with a block" do
            it "preserves the original" do
              list.send(method, &comparator)
              list.should == Hamster.list(*values)
            end

            it "returns #{expected.inspect}" do
              list.send(method, &comparator).should == Hamster.list(*expected)
            end
          end

          context "without a block" do
            it "preserves the original" do
              list.send(method)
              list.should eql(Hamster.list(*values))
            end

            it "returns #{expected.sort.inspect}" do
              list.send(method).should == Hamster.list(*expected.sort)
            end
          end
        end
      end
    end
  end
end