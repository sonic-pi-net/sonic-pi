require "spec_helper"
require "set"
require "hamster/set"

describe Hamster::Set do
  let(:set) { Hamster.set("A", "B", "C") }

  describe "#foreach" do
    let(:foreach) { set.foreach(&block) }

    context "without a block" do
      let(:block) { nil }

      it "returns self" do
        expect(foreach).to eq(set)
      end
    end

    context "with an empty block" do
      let(:block) { ->(item) {} }

      it "returns nil" do
        expect(foreach).to be(nil)
      end
    end

    context "with a block" do
      let(:items) { ::Set.new }
      let(:values) { ::Set.new(%w[A B C]) }
      let(:block) { ->(item) { items << item } }

      before(:each) { foreach }

      it "yields all values" do
        expect(items).to eq(values)
      end
    end
  end
end
