require "spec_helper"

require "hamster/stack"

describe Hamster::Stack do

  [:dup, :clone].each do |method|

    [
      [],
      ["A"],
      %w[A B C],
    ].each do |values|

      describe "on #{values.inspect}" do

        before do
          @stack = Hamster.stack(*values)
        end

        it "returns self" do
          @stack.send(method).should equal(@stack)
        end

      end

    end

  end

end
