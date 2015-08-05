require "spec_helper"
require "hamster/set"

describe Hamster::Set do
  describe "#join" do
    context "with a separator" do
      [
        [[], ""],
        [["A"], "A"],
        [[DeterministicHash.new("A", 1), DeterministicHash.new("B", 2), DeterministicHash.new("C", 3)], "A|B|C"]
      ].each do |values, expected|
        context "on #{values.inspect}" do
          let(:set) { Hamster.set(*values) }

          it "preserves the original" do
            set.join("|")
            set.should eql(Hamster.set(*values))
          end

          it "returns #{expected.inspect}" do
            set.join("|").should eql(expected)
          end
        end
      end
    end

    context "without a separator" do
      [
        [[], ""],
        [["A"], "A"],
        [[DeterministicHash.new("A", 1), DeterministicHash.new("B", 2), DeterministicHash.new("C", 3)], "ABC"]
      ].each do |values, expected|
        context "on #{values.inspect}" do
          let(:set) { Hamster.set(*values) }

          it "preserves the original" do
            set.join
            set.should eql(Hamster.set(*values))
          end

          it "returns #{expected.inspect}" do
            set.join.should eql(expected)
          end
        end
      end
    end

    context "without a separator (with global default separator set)" do
      before { $, = '**' }
      let(:set) { Hamster::Set[DeterministicHash.new("A", 1), DeterministicHash.new("B", 2), DeterministicHash.new("C", 3)] }
      after  { $, = nil }

      context "on ['A', 'B', 'C']" do
        it "preserves the original" do
          set.join
          set.should eql(Hamster::Set[DeterministicHash.new("A", 1), DeterministicHash.new("B", 2), DeterministicHash.new("C", 3)])
        end

        it "returns #{@expected.inspect}" do
          set.join.should == "A**B**C"
        end
      end
    end
  end
end