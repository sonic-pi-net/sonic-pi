require "spec_helper"

require "hamster/hash"

describe Hamster::Hash do

  describe "#inspect" do

    [
      [[], "{}"],
      [["A" => "aye"], "{\"A\" => \"aye\"}"],
      [[DeterministicHash.new("A", 1) => "aye", DeterministicHash.new("B", 2) => "bee", DeterministicHash.new("C", 3) => "see"], "{\"A\" => \"aye\", \"B\" => \"bee\", \"C\" => \"see\"}"]
    ].each do |values, expected|

      describe "on #{values.inspect}" do

        before do
          original = Hamster.hash(*values)
          @result = original.inspect
        end

        it "returns #{expected.inspect}" do
          @result.should == expected
        end

      end

    end

  end

end
