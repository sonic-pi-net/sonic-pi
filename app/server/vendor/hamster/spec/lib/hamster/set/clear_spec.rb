require "spec_helper"
require "hamster/set"

describe Hamster::Set do
  describe "#clear" do
    [
      [],
      ["A"],
      %w[A B C],
    ].each do |values|
      describe "on #{values}" do
        let(:set) { Hamster.set(*values) }

        it "preserves the original" do
          set.clear
          set.should eql(Hamster.set(*values))
        end

        it "returns an empty set" do
          set.clear.should equal(Hamster.set)
        end
      end
    end

    context "from a subclass" do
      it "returns an empty instance of the subclass" do
        subclass = Class.new(Hamster::Set)
        instance = subclass.new([:a, :b, :c, :d])
        instance.clear.class.should be(subclass)
        instance.clear.should be_empty
      end
    end
  end
end