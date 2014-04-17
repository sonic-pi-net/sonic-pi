require "spec_helper"

require "hamster/stack"

describe Hamster::Stack do

  [:peek, :top].each do |method|

    describe "##{method}" do

      [
        [[], nil],
        [["A"], "A"],
        [%w[A B C], "C"],
      ].each do |values, expected|

        describe "on #{values.inspect}" do

          before do
            @original = Hamster.stack(*values)
            @result = @original.send(method)
          end

          it "preserves the original" do
            @original.should == Hamster.stack(*values)
          end

          it "returns #{expected.inspect}" do
            @result.should == expected
          end

        end

      end

    end

  end

end
