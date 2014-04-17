require "spec_helper"

require "hamster/vector"

describe Hamster::Vector do

  [:first, :head].each do |method|

    describe "##{method}" do

      [
        [[], nil],
        [["A"], "A"],
        [%w[A B C], "A"],
      ].each do |values, expected|

        describe "on #{values.inspect}" do

          before do
            @vector = Hamster.vector(*values)
          end

          it "returns #{expected.inspect}" do
            @vector.send(method).should == expected
          end

        end

      end

    end

  end

end
