require "spec_helper"
require "hamster/set"

describe Hamster::Set do
  [:maximum, :max].each do |method|
    describe "##{method}" do
      context "with a block" do
        [
          [[], nil],
          [["A"], "A"],
          [%w[Ichi Ni San], "Ichi"],
        ].each do |values, expected|
          describe "on #{values.inspect}" do
            let(:set) { Hamster.set(*values) }
            let(:result) { set.send(method) { |maximum, item| maximum.length <=> item.length }}

            it "returns #{expected.inspect}" do
              result.should == expected
            end
          end
        end
      end

      context "without a block" do
        [
          [[], nil],
          [["A"], "A"],
          [%w[Ichi Ni San], "San"],
        ].each do |values, expected|
          describe "on #{values.inspect}" do
            it "returns #{expected.inspect}" do
              Hamster.set(*values).send(method).should == expected
            end
          end
        end
      end
    end
  end
end