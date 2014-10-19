require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do
  [:first, :head].each do |method|
    describe "##{method}" do
      [
        [[], nil],
        [["A"], "A"],
        [%w[A B C], "A"],
        [(1..32), 1],
      ].each do |values, expected|
        describe "on #{values.inspect}" do
          it "returns #{expected.inspect}" do
            Hamster.vector(*values).send(method).should == expected
          end
        end
      end
    end
  end
end