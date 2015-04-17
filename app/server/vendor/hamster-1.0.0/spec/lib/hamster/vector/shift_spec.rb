require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do
  describe "#shift" do
    [
      [[], []],
      [["A"], []],
      [%w[A B C], %w[B C]],
      [1..31, 2..31],
      [1..32, 2..32],
      [1..33, 2..33]
    ].each do |values, expected|
      context "on #{values.inspect}" do
        let(:vector) { Hamster.vector(*values) }

        it "preserves the original" do
          vector.shift
          vector.should eql(Hamster.vector(*values))
        end

        it "returns #{expected.inspect}" do
          vector.shift.should eql(Hamster.vector(*expected))
        end
      end
    end
  end
end