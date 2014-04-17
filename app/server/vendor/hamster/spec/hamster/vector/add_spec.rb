require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do
  let(:vector) { Hamster.vector(*values) }

  describe "#add" do
    let(:add) { vector.add(added_value) }

    shared_examples "checking adding values" do
      let(:added_vector) { Hamster.vector(*added_values) }

      it "preserves the original" do
        orignal = vector
        vector.add(added_value)
        expect(orignal).to eq(vector)
      end

      it "adds the item to the vector" do
        expect(add).to eq(added_vector)
      end
    end

    context "with a empty array adding a single item" do
      let(:values) { [] }
      let(:added_value) { "A" }
      let(:added_values) { ["A"] }

      include_examples "checking adding values"
    end

    context "with a single-item array adding a different item" do
      let(:values) { ["A"] }
      let(:added_value) { "B" }
      let(:added_values) { %w[A B] }

      include_examples "checking adding values"
    end

    context "with a single-item array adding a duplicate item" do
      let(:values) { ["A"] }
      let(:added_value) { "A" }
      let(:added_values) { %w[A A] }

      include_examples "checking adding values"
    end
  end
end
