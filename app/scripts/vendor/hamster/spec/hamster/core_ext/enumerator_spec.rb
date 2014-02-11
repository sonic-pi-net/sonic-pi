require "spec_helper"
require "hamster/core_ext/enumerator"

describe Enumerator do
  let(:enumerator) { %w[A B C].to_enum }

  describe "#to_list" do
    let(:to_list) { enumerator.to_list }

    it "returns an equivalent list" do
      expect(to_list).to eq(Hamster.list("A", "B", "C"))
    end

    it "is lazy" do
      expect(to_list.head).to eq("A")
      expect(enumerator.next).to eq("B")
    end
  end
end
