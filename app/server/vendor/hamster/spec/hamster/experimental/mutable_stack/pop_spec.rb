require "spec_helper"

require "hamster/experimental/mutable_stack"

describe Hamster::MutableStack do

  [:pop, :dequeue].each do |method|

    describe "##{method}" do

      [
        [[], nil, []],
        [["A"], "A", []],
        [%w[A B], "B", ["A"]],
        [%w[A B C], "C", %w[A B]],
      ].each do |initial_state, return_value, resulting_state|

        describe "on #{initial_state.inspect}" do

          before do
            @stack = Hamster.mutable_stack(*initial_state)
            @result = @stack.send(method)
          end

          it "returns #{return_value.inspect}" do
            @result.should == return_value
          end

          it "modifies the stack to #{resulting_state.inspect}" do
            @stack.should == Hamster.mutable_stack(*resulting_state)
          end

        end

      end

    end

  end

end
