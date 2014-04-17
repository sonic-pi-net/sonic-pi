require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do
  let(:vector) { Hamster.vector(*values) }

  describe "#inspect" do
    let(:inspect) { vector.inspect }

    shared_examples "checking output" do
      it "returns the array as a inspected string" do
        expect(inspect).to eq(output)
      end
    end

    context "with an empty array" do
      let(:output) { "[]" }
      let(:values) { [] }
    end

    context "with a single item array" do
      let(:output) { "[\"A\"]" }
      let(:values) { %w[A] }
    end

    context "with a mulit-item array" do
      let(:output) { "[\"A\", \"B\"]" }
      let(:values) { %w[A B] }

      include_examples "checking output"
    end
  end
end
