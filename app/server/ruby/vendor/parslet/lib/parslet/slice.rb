
# A slice is a small part from the parse input. A slice mainly behaves like
# any other string, except that it remembers where it came from (offset in
# original input).
#
# == Extracting line and column
#
# Using the #line_and_column method, you can extract the line and column in
# the original input where this slice starts.
#
# Example:
#   slice.line_and_column # => [1, 13]
#   slice.offset          # => 12
#
# == Likeness to strings
#
# Parslet::Slice behaves in many ways like a Ruby String. This likeness
# however is not complete - many of the myriad of operations String supports
# are not yet in Slice. You can always extract the internal string instance by
# calling #to_s.
#
# These omissions are somewhat intentional. Rather than maintaining a full
# delegation, we opt for a partial emulation that gets the job done.
#
class Parslet::Slice
  attr_reader :str, :offset
  attr_reader :line_cache

  # Construct a slice using a string, an offset and an optional line cache. 
  # The line cache should be able to answer to the #line_and_column message. 
  #
  def initialize(string, offset, line_cache=nil)
    @str, @offset = string, offset
    @line_cache = line_cache
  end

  # Compares slices to other slices or strings.
  #
  def == other
    str == other
  end

  # Match regular expressions.
  #
  def match(regexp)
    str.match(regexp)
  end

  # Returns the slices size in characters.
  #
  def size
    str.size
  end
  
  # Concatenate two slices; it is assumed that the second slice begins 
  # where the first one ends. The offset of the resulting slice is the same
  # as the one of this slice. 
  #
  def +(other)
    self.class.new(str + other.to_s, offset, line_cache)
  end

  # Returns a <line, column> tuple referring to the original input.
  #
  def line_and_column
    raise ArgumentError, "No line cache was given, cannot infer line and column." \
      unless line_cache

    line_cache.line_and_column(self.offset)
  end


  # Conversion operators -----------------------------------------------------
  def to_str
    str
  end
  alias to_s to_str

  def to_slice
    self
  end
  def to_sym
    str.to_sym
  end
  def to_int
    Integer(str)
  end
  def to_i
    str.to_i
  end
  def to_f
    str.to_f
  end

  # Inspection & Debugging ---------------------------------------------------

  # Prints the slice as <code>"string"@offset</code>.
  def inspect
    str.inspect << "@#{offset}"
  end
end