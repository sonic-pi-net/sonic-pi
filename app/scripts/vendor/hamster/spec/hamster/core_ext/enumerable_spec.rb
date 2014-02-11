require "spec_helper"
require "hamster/core_ext/enumerable"

describe Enumerable do
  class TestEnumerable
    include Enumerable

    def initialize(*values)
      @values = values
    end

    def each(&block)
      @values.each(&block)
    end
  end
  let(:enumerable) { TestEnumerable.new("A", "B", "C") }

  describe "#to_list" do
    let(:to_list) { enumerable.to_list }

    it "returns an equivalent list" do
      expect(to_list).to eq(Hamster.list("A", "B", "C"))
    end
  end
end
