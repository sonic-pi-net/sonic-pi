require "spec_helper"
require "hamster/set"

describe Hamster::Set do
  [
    [:sort, ->(left, right) { left.length <=> right.length }],
    [:sort_by, ->(item) { item.length }],
  ].each do |method, comparator|
    describe "##{method}" do
      [
        [[], []],
        [["A"], ["A"]],
        [%w[Ichi Ni San], %w[Ni San Ichi]],
      ].each do |values, expected|
        describe "on #{values.inspect}" do
          let(:set) { Hamster.set(*values) }

          describe "with a block" do
            let(:result) { set.send(method, &comparator) }

            it "returns #{expected.inspect}" do
              result.should eql(Hamster.sorted_set(*expected, &comparator))
              result.to_a.should == expected
            end

            it "doesn't change the original Set" do
              result
              set.should eql(Hamster::Set.new(values))
            end
          end

          describe "without a block" do
            let(:result) { set.send(method) }

            it "returns #{expected.sort.inspect}" do
              result.should eql(Hamster.sorted_set(*expected))
              result.to_a.should == expected.sort
            end

            it "doesn't change the original Set" do
              result
              set.should eql(Hamster::Set.new(values))
            end
          end
        end
      end
    end
  end
end