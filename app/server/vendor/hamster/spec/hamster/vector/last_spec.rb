require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do
  let(:vector) { Hamster.vector(*values) }

  describe "#last" do
    let(:last) { vector.last }

    shared_examples "checking values" do
      it "returns the last item" do
        expect(last).to eq(last_item)
      end
    end

    context "with an empty array" do
      let(:last_item) { nil }
      let(:values) { [] }

      include_examples "checking values"
    end

    context "with a single item array" do
      let(:last_item) { "A" }
      let(:values) { %w[A] }

      include_examples "checking values"
    end

    context "with a multi-item array" do
      let(:last_item) { "B" }
      let(:values) { %w[A B] }

      include_examples "checking values"
    end
  end
end
