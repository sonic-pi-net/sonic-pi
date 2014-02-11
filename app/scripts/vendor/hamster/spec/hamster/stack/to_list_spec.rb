require "spec_helper"

require "hamster/stack"
require "hamster/list"

describe Hamster::Stack do

  describe "#to_list" do

    [
      [[], []],
      [["A"], ["A"]],
      [%w[A B C], %w[C B A]],
    ].each do |values, expected|

      describe "on #{values.inspect}" do

        before do
          @stack = Hamster.stack(*values)
          @result = @stack.to_list
        end

        it "returns #{expected.inspect}" do
          @result.should == Hamster.list(*expected)
        end

      end

    end

  end

end
