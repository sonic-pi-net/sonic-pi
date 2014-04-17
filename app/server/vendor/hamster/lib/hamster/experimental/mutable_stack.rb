require "forwardable"
require "hamster/stack"
require "hamster/read_copy_update"

module Hamster
  def self.mutable_stack(*items)
    MutableStack.new(stack(*items))
  end

  class MutableStack
    extend Forwardable
    include ReadCopyUpdate

    def push(item)
      transform { |stack| stack.push(item) }
    end
    def_delegator :self, :push, :<<
    def_delegator :self, :push, :enqueue

    def pop
      top = nil
      transform do |stack|
        top = stack.peek
        stack.pop
      end
      top
    end
    def_delegator :self, :pop, :dequeue
  end
end
