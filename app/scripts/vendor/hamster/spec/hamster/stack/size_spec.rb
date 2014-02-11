require "spec_helper"

require "hamster/stack"

describe Hamster::Stack do

  [:size, :length].each do |method|

    describe "##{method}" do

      [
        [[], 0],
        [["A"], 1],
        [%w[A B C], 3],
      ].each do |values, expected|

        describe "on #{values.inspect}" do

          before do
            @stack = Hamster.stack(*values)
          end

          it "returns #{expected.inspect}" do
            @stack.send(method).should == expected
          end

        end

      end

    end

  end

end
