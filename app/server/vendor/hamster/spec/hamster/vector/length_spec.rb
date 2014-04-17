require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do
  let(:vector) { Hamster.vector(*values) }

  describe "#length" do
    let(:length) { vector.length }

    shared_examples "checking size" do
      it "returns the values" do
        expect(length).to eq(size)
      end
    end

    context "with an empty array" do
      let(:values) { [] }
      let(:size) { 0 }

      include_examples "checking size"
    end

    context "with a single item array" do
      let(:values) { %w[A] }
      let(:size) { 1 }

      include_examples "checking size"
    end

    context "with a multi-item array" do
      let(:values) { %w[A B] }
      let(:size) { 2 }

      include_examples "checking size"
    end
  end
end
