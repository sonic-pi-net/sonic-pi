require "spec_helper"

require "hamster/experimental/mutable_stack"

describe Hamster::MutableStack do

  [:push, :<<, :enqueue].each do |method|

    describe "##{method}" do

      [
        [[], "A", ["A"]],
        [["A"], "B", %w[A B]],
        [["A"], "A", %w[A A]],
        [%w[A B C], "D", %w[A B C D]],
      ].each do |initial_state, new_value, resulting_state|

        describe "on #{initial_state.inspect} with #{new_value.inspect}" do

          before do
            @stack = Hamster.mutable_stack(*initial_state)
            @result = @stack.send(method, new_value)
          end

          it "returns self" do
            @result.should equal(@stack)
          end

          it "modifies the stack to #{resulting_state.inspect}" do
            @stack.should == Hamster.mutable_stack(*resulting_state)
          end

        end

      end

    end

  end

end
