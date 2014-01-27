
# Used internally for representing a bind placeholder in a Parslet::Transform
# pattern. This is the superclass for all bindings. 
#
# It defines the most permissive kind of bind, the one that matches any subtree
# whatever it looks like. 
#
class Parslet::Pattern::SubtreeBind < Struct.new(:symbol)
  def variable_name
    symbol
  end
  
  def inspect
    "#{bind_type_name}(#{symbol.inspect})"
  end
  
  def can_bind?(subtree)
    true
  end

private 
  def bind_type_name 
    if md=self.class.name.match(/(\w+)Bind/)
      md.captures.first.downcase
    else
      # This path should never be used, but since this is for inspection only, 
      # let's not raise.
      'unknown_bind'
    end
  end
end

# Binds a symbol to a simple subtree, one that is not either a sequence of
# elements or a collection of attributes. 
#
class Parslet::Pattern::SimpleBind < Parslet::Pattern::SubtreeBind
  def can_bind?(subtree)
    not [Hash, Array].include?(subtree.class)
  end
end

# Binds a symbol to a sequence of simple leafs ([element1, element2, ...])
#
class Parslet::Pattern::SequenceBind < Parslet::Pattern::SubtreeBind
  def can_bind?(subtree)
    subtree.kind_of?(Array) &&
      (not subtree.any? { |el| [Hash, Array].include?(el.class) })
  end
end