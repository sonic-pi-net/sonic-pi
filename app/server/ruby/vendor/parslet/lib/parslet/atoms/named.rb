# Names a match to influence tree construction. 
#
# Example: 
#
#   str('foo')            # will return 'foo', 
#   str('foo').as(:foo)   # will return :foo => 'foo'
#
class Parslet::Atoms::Named < Parslet::Atoms::Base
  attr_reader :parslet, :name
  def initialize(parslet, name)
    super()

    @parslet, @name = parslet, name
  end
  
  def apply(source, context, consume_all)
    success, value = result = parslet.apply(source, context, consume_all)

    return result unless success
    succ(
      produce_return_value(
        value))
  end
  
  def to_s_inner(prec)
    "#{name}:#{parslet.to_s(prec)}"
  end
private
  def produce_return_value(val)
    { name => flatten(val, true) }
  end
end
