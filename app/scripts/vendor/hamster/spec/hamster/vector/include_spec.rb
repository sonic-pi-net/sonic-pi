require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do

  [:include?, :member?, :contains?, :elem?].each do |method|

    describe "##{method}" do

      [
        [[], "A", false],
        [[], nil, false],
        [["A"], "A", true],
        [["A"], "B", false],
        [["A"], nil, false],
        [["A", "B", nil], "A", true],
        [["A", "B", nil], "B", true],
        [["A", "B", nil], nil, true],
        [["A", "B", nil], "C", false],
        [[2], 2, true],
        [[2], 2.0, true],
        [[2.0], 2.0, true],
        [[2.0], 2, true],
      ].each do |values, item, expected|

        describe "on #{values.inspect}" do

          before do
            @vector = Hamster.vector(*values)
          end

          it "returns #{expected.inspect}" do
            @vector.send(method, item).should == expected
          end

        end

      end

    end

  end

end
