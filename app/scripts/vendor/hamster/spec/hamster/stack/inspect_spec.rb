require "spec_helper"

require "hamster/stack"

describe Hamster::Stack do

  describe "#inspect" do

    [
      [[], "[]"],
      [["A"], "[\"A\"]"],
      [%w[A B C], "[\"C\", \"B\", \"A\"]"]
    ].each do |values, expected|

      describe "on #{values.inspect}" do

        before do
          @stack = Hamster.stack(*values)
        end

        it "returns #{expected.inspect}" do
          @stack.inspect.should == expected
        end

      end

    end

  end

end
