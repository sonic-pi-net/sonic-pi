require "spec_helper"

require "hamster/stack"

describe Hamster::Stack do

  describe "#empty?" do

    [
      [[], true],
      [["A"], false],
      [%w[A B C], false],
    ].each do |values, expected|

      describe "on #{values.inspect}" do

        before do
          @stack = Hamster.stack(*values)
        end

        it "returns #{expected.inspect}" do
          @stack.empty?.should == expected
        end

      end

    end

  end

end
