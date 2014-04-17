require "spec_helper"
require "hamster/list"

describe Hamster::List do
  let(:list) { Hamster.list(*values) }

  describe "#pop" do
    let(:pop) { list.pop }

    context "with an empty list" do
      let(:values) { [] }

      it "returns an empty list" do
        expect(pop).to eq(Hamster.list)
      end
    end

    context "with a list with a few items" do
      let(:values) { %w[a b c] }

      it "should remove the last item" do
        expect(pop).to eq(Hamster.list("a", "b"))
      end
    end
  end
end
