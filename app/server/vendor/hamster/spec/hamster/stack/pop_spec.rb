require "spec_helper"

require "hamster/stack"

describe Hamster::Stack do

  [:pop, :dequeue].each do |method|

    describe "##{method}" do

      [
        [[], []],
        [["A"], []],
        [%w[A B], ["A"]],
        [%w[A B C], %w[A B]],
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
            @result.should == Hamster.stack(*expected)
          end

        end

      end

    end

  end

end
