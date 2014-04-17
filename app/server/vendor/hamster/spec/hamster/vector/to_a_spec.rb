require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do
  let(:vector) { Hamster.vector(*values) }

  describe "#to_a" do
    let(:to_a) { vector.to_a }

    shared_examples "checking to_a values" do
      it "returns the values" do
        expect(to_a).to eq(values)
      end

      it "doesn't end with 'The End'" do
        expect(to_a).to_not include("The End")
      end

      it "returns a mutable array" do
        to_a << "The End"
        expect(to_a).to include("The End")
      end
    end

    context "with an empty array" do
      let(:values) { [] }

      include_examples "checking to_a values"
    end

    context "with an single item array" do
      let(:values) { %w[A] }

      include_examples "checking to_a values"
    end

    context "with an multi-item array" do
      let(:values) { %w[A B] }

      include_examples "checking to_a values"
    end
  end
end
