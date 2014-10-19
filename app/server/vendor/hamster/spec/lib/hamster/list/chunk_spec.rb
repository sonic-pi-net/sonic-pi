require "spec_helper"
require "hamster/list"

describe Hamster::List do
  describe "#chunk" do
    it "is lazy" do
      -> { Hamster.stream { fail }.chunk(2) }.should_not raise_error
    end

    [
      [[], []],
      [["A"], [Hamster.list("A")]],
      [%w[A B C], [Hamster.list("A", "B"), Hamster.list("C")]],
    ].each do |values, expected|
      context "on #{values.inspect}" do
        let(:list) { Hamster.list(*values) }

        it "preserves the original" do
          list.chunk(2)
          list.should eql(Hamster.list(*values))
        end

        it "returns #{expected.inspect}" do
          list.chunk(2).should eql(Hamster.list(*expected))
        end
      end
    end
  end
end