
# Stores the result of matching an atom against input in the #captures in 
# parse context. Doing so will allow you to pull parts of the ongoing parse
# out later and use them to match other pieces of input. 
#
# Example: 
#   # After this, context.captures[:an_a] returns 'a'
#   str('a').capture(:an_a)
#
#   # Capture and use of the capture: (matches either 'aa' or 'bb')
#   match['ab'].capture(:first) >> 
#     dynamic { |src, ctx| str(ctx.captures[:first]) }
#   
class Parslet::Atoms::Capture < Parslet::Atoms::Base
  attr_reader :parslet, :name

  def initialize(parslet, name)
    super()

    @parslet, @name = parslet, name
  end

  def apply(source, context, consume_all)
    success, value = result = parslet.apply(source, context, consume_all)

    if success
      context.captures[name.to_sym] = 
        flatten(value)
    end
    
    return result
  end
  
  def to_s_inner(prec)
    "(#{name.inspect} = #{parslet.to_s(prec)})"
  end
end

