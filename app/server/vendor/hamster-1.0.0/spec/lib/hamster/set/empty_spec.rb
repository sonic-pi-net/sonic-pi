require "spec_helper"
require "hamster/set"

describe Hamster::Set do
  describe "#empty?" do
    [
      [[], true],
      [["A"], false],
      [%w[A B C], false],
      [[nil], false],
      [[false], false]
    ].each do |values, expected|
      describe "on #{values.inspect}" do
        it "returns #{expected.inspect}" do
          Hamster.set(*values).empty?.should == expected
        end
      end
    end
  end

  describe ".empty" do
    it "returns the canonical empty set" do
      Hamster::Set.empty.should be_empty
      Hamster::Set.empty.object_id.should be(Hamster::Set.empty.object_id)
      Hamster::Set.empty.should be(Hamster::EmptySet)
    end

    context "from a subclass" do
      it "returns an empty instance of the subclass" do
        subclass = Class.new(Hamster::Set)
        subclass.empty.class.should be(subclass)
        subclass.empty.should be_empty
      end

      it "calls overridden #initialize when creating empty Set" do
        subclass = Class.new(Hamster::Set) do
          def initialize
            @variable = 'value'
          end
        end
        subclass.empty.instance_variable_get(:@variable).should == 'value'
      end
    end
  end
end