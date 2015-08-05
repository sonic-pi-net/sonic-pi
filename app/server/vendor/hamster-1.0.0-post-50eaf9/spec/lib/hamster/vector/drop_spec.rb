require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do
  describe "#drop" do
    [
      [[], 10, []],
      [["A"], 10, []],
      [["A"], 1, []],
      [["A"], 0, ["A"]],
      [%w[A B C], 0, %w[A B C]],
      [%w[A B C], 2, ["C"]],
      [(1..32), 3, (4..32)],
      [(1..33), 32, [33]]
    ].each do |values, number, expected|
      describe "#{number} from #{values.inspect}" do
        let(:vector) { Hamster.vector(*values) }

        it "preserves the original" do
          vector.drop(number)
          vector.should eql(Hamster.vector(*values))
        end

        it "returns #{expected.inspect}" do
          vector.drop(number).should eql(Hamster.vector(*expected))
        end
      end
    end

    it "raises an ArgumentError if number of elements specified is negative" do
      -> { V[1, 2, 3].drop(-1) }.should raise_error(ArgumentError)
      -> { V[1, 2, 3].drop(-3) }.should raise_error(ArgumentError)
    end
  end
end