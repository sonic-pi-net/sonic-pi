require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do
  describe "#take" do
    [
      [[], 10, []],
      [["A"], 10, ["A"]],
      [%w[A B C], 0, []],
      [%w[A B C], 2, %w[A B]],
      [(1..32), 1, [1]],
      [(1..33), 32, (1..32)],
      [(1..100), 40, (1..40)]
    ].each do |values, number, expected|
      describe "#{number} from #{values.inspect}" do
        let(:vector) { Hamster.vector(*values) }

        it "preserves the original" do
          vector.take(number)
          vector.should eql(Hamster.vector(*values))
        end

        it "returns #{expected.inspect}" do
          vector.take(number).should eql(Hamster.vector(*expected))
        end
      end
    end
  end
end