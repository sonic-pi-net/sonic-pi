require "spec_helper"
require "hamster/list"

describe Hamster::List do
  describe "#intersperse" do
    it "is lazy" do
      -> { Hamster.stream { fail }.intersperse("") }.should_not raise_error
    end

    [
      [[], []],
      [["A"], ["A"]],
      [%w[A B C], ["A", "|", "B", "|", "C"]]
    ].each do |values, expected|
      context "on #{values.inspect}" do
        let(:list) { Hamster.list(*values) }

        it "preserves the original" do
          list.intersperse("|")
          list.should eql(Hamster.list(*values))
        end

        it "returns #{expected.inspect}" do
          list.intersperse("|").should eql(Hamster.list(*expected))
        end
      end
    end
  end
end