require "spec_helper"

require "hamster/set"

describe Hamster::Set do

  describe "#inspect" do

    [
      [[], "{}"],
      [["A"], "{\"A\"}"],
      [[DeterministicHash.new("A", 1), DeterministicHash.new("B", 2), DeterministicHash.new("C", 3)], "{\"A\", \"B\", \"C\"}"]
    ].each do |values, expected|

      describe "on #{values.inspect}" do

        before do
          original = Hamster.set(*values)
          @result = original.inspect
        end

        it "returns #{expected.inspect}" do
          @result.should == expected
        end

      end

    end

  end

end
