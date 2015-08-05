require "spec_helper"
require "hamster/list"

describe Hamster::List do
  describe "#uniq" do
    it "is lazy" do
      -> { Hamster.stream { fail }.uniq }.should_not raise_error
    end

    [
      [[], []],
      [["A"], ["A"]],
      [%w[A B C], %w[A B C]],
      [%w[A B A C C], %w[A B C]],
    ].each do |values, expected|
      context "on #{values.inspect}" do
        let(:list) { Hamster.list(*values) }

        it "preserves the original" do
          list.uniq
          list.should eql(Hamster.list(*values))
        end

        it "returns #{expected.inspect}" do
          list.uniq.should eql(Hamster.list(*expected))
        end
      end
    end
  end
end