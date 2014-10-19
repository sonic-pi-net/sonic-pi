require "spec_helper"
require "hamster/sorted_set"
require "hamster/set"

describe Hamster::SortedSet do
  describe "#to_set" do
    [
      [],
      ["A"],
      %w[A B C],
    ].each do |values|
      context "on #{values.inspect}" do
        it "returns a set with the same values" do
          Hamster.sorted_set(*values).to_set.should eql(Hamster.set(*values))
        end
      end
    end
  end
end