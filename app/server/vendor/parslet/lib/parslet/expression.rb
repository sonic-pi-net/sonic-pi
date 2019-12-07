
# Allows specifying rules as strings using the exact same grammar that treetop
# does, minus the actions. This is on one hand a good example of a fully
# fledged parser and on the other hand might even turn out really useful. 
#
# This can be viewed as an extension to parslet and might even be hosted in
# its own gem one fine day. 
# 
class Parslet::Expression
  include Parslet
  
  autoload :Treetop, 'parslet/expression/treetop'
  
  # Creates a parslet from a foreign language expression. 
  #
  # Example: 
  #   
  #   Parslet::Expression.new("'a' 'b'")
  #
  def initialize(str, opts={}, context=self)
    @type = opts[:type] || :treetop
    @exp = str
    @parslet = transform(
      parse(str))
  end
  
  # Transforms the parse tree into a parslet expression. 
  #
  def transform(tree)
    transform = Treetop::Transform.new
    
    # pp tree
    transform.apply(tree)
  rescue 
    warn "Could not transform: " + tree.inspect
    raise
  end
  
  # Parses the string and returns a parse tree.
  #
  def parse(str)
    parser = Treetop::Parser.new
    parser.parse(str)
  end

  # Turns this expression into a parslet.
  #
  def to_parslet
    @parslet
  end
end