require "spec_helper"
require "hamster/vector"

describe Hamster::Vector do

  [:empty?, :null?].each do |method|

    describe "##{method}" do

      [
        [[], true],
        [["A"], false],
        [%w[A B C], false],
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
