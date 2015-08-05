require "spec_helper"
require "hamster/set"

describe Hamster do
  describe "#flatten" do
    [
      [["A"], ["A"]],
      [%w[A B C], %w[A B C]],
      [["A", Hamster.set("B"), "C"], %w[A B C]],
      [[Hamster.set("A"), Hamster.set("B"), Hamster.set("C")], %w[A B C]],
    ].each do |values, expected|
      describe "on #{values}" do
        let(:set) { Hamster.set(*values) }

        it "preserves the original" do
          set.flatten
          set.should eql(Hamster.set(*values))
        end

        it "returns the inlined values" do
          set.flatten.should eql(Hamster.set(*expected))
        end
      end
    end

    context "on an empty set" do
      it "returns an empty set" do
        Hamster.set.flatten.should equal(Hamster.set)
      end
    end

    context "on a set with multiple levels of nesting" do
      it "inlines lower levels of nesting" do
        set = Hamster.set(Hamster.set(Hamster.set(1)), Hamster.set(Hamster.set(2)))
        set.flatten.should eql(Hamster.set(1, 2))
      end
    end

    context "from a subclass" do
      it "returns an instance of the subclass" do
        subclass = Class.new(Hamster::Set)
        subclass.new.flatten.class.should be(subclass)
        subclass.new([Hamster.set(1), Hamster.set(2)]).flatten.class.should be(subclass)
      end
    end
  end
end