require "spec_helper"

require "hamster/stack"

describe Hamster::Stack do

  [:push, :<<, :enqueue].each do |method|

    describe "##{method}" do

      [
        [[], "A", ["A"]],
        [["A"], "B", %w[A B]],
        [["A"], "A", %w[A A]],
        [%w[A B C], "D", %w[A B C D]],
      ].each do |values, new_value, expected|

        describe "on #{values.inspect} with #{new_value.inspect}" do

          before do
            @original = Hamster.stack(*values)
            @result = @original.send(method, new_value)
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
