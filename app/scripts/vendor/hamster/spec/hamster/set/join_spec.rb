require "spec_helper"

require "hamster/set"

describe Hamster::Set do

  describe "#join" do

    describe "with a separator" do

      [
        [[], ""],
        [["A"], "A"],
        [[DeterministicHash.new("A", 1), DeterministicHash.new("B", 2), DeterministicHash.new("C", 3)], "A|B|C"]
      ].each do |values, expected|

        describe "on #{values.inspect}" do

          before do
            @original = Hamster.set(*values)
            @result = @original.join("|")
          end

          it "preserves the original" do
            @original.should == Hamster.set(*values)
          end

          it "returns #{expected.inspect}" do
            @result.should == expected
          end

        end

      end

    end

    describe "without a separator" do

      [
        [[], ""],
        [["A"], "A"],
        [[DeterministicHash.new("A", 1), DeterministicHash.new("B", 2), DeterministicHash.new("C", 3)], "ABC"]
      ].each do |values, expected|

        describe "on #{values.inspect}" do

          before do
            @original = Hamster.set(*values)
            @result = @original.join
          end

          it "preserves the original" do
            @original.should == Hamster.set(*values)
          end

          it "returns #{expected.inspect}" do
            @result.should == expected
          end

        end

      end

    end

  end

end
