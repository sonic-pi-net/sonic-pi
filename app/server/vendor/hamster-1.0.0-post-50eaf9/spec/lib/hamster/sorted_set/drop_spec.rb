require "spec_helper"
require "hamster/sorted_set"

describe Hamster::SortedSet do
  describe "#drop" do
    [
      [[], 0, []],
      [[], 10, []],
      [["A"], 10, []],
      [%w[A B C], 0, %w[A B C]],
      [%w[A B C], 1, %w[B C]],
      [%w[A B C], 2, ["C"]],
      [%w[A B C], 3, []]
    ].each do |values, number, expected|
      context "#{number} from #{values.inspect}" do
        let(:sorted_set) { Hamster.sorted_set(*values) }

        it "preserves the original" do
          sorted_set.drop(number)
          sorted_set.should eql(Hamster.sorted_set(*values))
        end

        it "returns #{expected.inspect}" do
          sorted_set.drop(number).should eql(Hamster.sorted_set(*expected))
        end
      end
    end
  end
end