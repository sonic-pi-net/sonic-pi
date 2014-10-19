require "spec_helper"
require "hamster/list"

describe Hamster do
  describe "#flatten" do
    it "is lazy" do
      -> { Hamster.stream { fail }.flatten }.should_not raise_error
    end

    [
      [[], []],
      [["A"], ["A"]],
      [%w[A B C], %w[A B C]],
      [["A", Hamster.list("B"), "C"], %w[A B C]],
      [[Hamster.list("A"), Hamster.list("B"), Hamster.list("C")], %w[A B C]],
    ].each do |values, expected|
      context "on #{values}" do
        let(:list) { Hamster.list(*values) }

        it "preserves the original" do
          list.flatten
          list.should eql(Hamster.list(*values))
        end

        it "returns an empty list" do
          list.flatten.should eql(Hamster.list(*expected))
        end
      end
    end
  end
end