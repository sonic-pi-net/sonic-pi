require "hamster/list"

# Ruby's built-in `Enumerable` module.
# @see http://www.ruby-doc.org/core/Enumerable.html
module Enumerable
  # Return a new {Hamster::List} populated with the items in this `Enumerable` object.
  # @return [List]
  def to_list
    # use destructive operations to build up a new list, like Common Lisp's NCONC
    # this is a very fast way to build up a linked list
    list = tail = Hamster::Cons.allocate
    each do |item|
      new_node = Hamster::Cons.allocate
      new_node.instance_variable_set(:@head, item)
      tail.instance_variable_set(:@tail, new_node)
      tail = new_node
    end
    tail.instance_variable_set(:@tail, Hamster::EmptyList)
    list.tail
  end
end