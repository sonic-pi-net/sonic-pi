# Matches a string of characters. 
#
# Example: 
# 
#   str('foo') # matches 'foo'
#
class Parslet::Atoms::Str < Parslet::Atoms::Base
  attr_reader :str
  def initialize(str)
    super()

    @str = str.to_s
    @pat = Regexp.new(Regexp.escape(str))
    @len = str.size
    @error_msgs = {
      :premature  => "Premature end of input", 
      :failed     => "Expected #{str.inspect}, but got "
    }
  end
  
  def try(source, context, consume_all)
    return succ(source.consume(@len)) if source.matches?(@pat)
    
    # Input ending early:
    return context.err(self, source, @error_msgs[:premature]) \
      if source.chars_left<@len
    
    # Expected something, but got something else instead:  
    error_pos = source.pos  
    return context.err_at(
      self, source, 
      [@error_msgs[:failed], source.consume(@len)], error_pos) 
  end
  
  def to_s_inner(prec)
    "'#{str}'"
  end
end

