require "spec_helper"
require "hamster/list"

describe Hamster::List do
  let(:list) { Hamster.list(*values) }
  let(:filtered_list) { Hamster.list(*filtered_values) }

  describe "#filter" do
    it "is lazy" do
      expect { Hamster.stream { fail }.filter { |item| false } }.to_not raise_error
    end

    shared_examples "checking values" do
      context "with a block" do
        let(:filter) { list.filter { |item| item == item.upcase } }

        it "preserves the original" do
          expect(list).to eq(Hamster.list(*values))
        end

        it "returns the filtered list" do
          expect(filter).to eq(filtered_list)
        end
      end

      context "without a block" do
        let(:filter) { list.filter }

        it "returns itself" do
          expect(filter).to eq(list)
        end
      end
    end

    context "with an empty array" do
      let(:values) { [] }
      let(:filtered_values) { [] }

      include_examples "checking values"
    end

    context "with a single item array" do
      let(:values) { ["A"] }
      let(:filtered_values) { ["A"] }

      include_examples "checking values"
    end

    context "with a multi-item array" do
      let(:values) { %w[A B] }
      let(:filtered_values) { %w[A B] }

      include_examples "checking values"
    end

    context "with a multi-item single filterable array" do
      let(:values) { %w[A b] }
      let(:filtered_values) { ["A"] }

      include_examples "checking values"
    end

    context "with a multi-item multi-filterable array" do
      let(:values) { %w[a b] }
      let(:filtered_values) { [] }

      include_examples "checking values"
    end
  end
end
