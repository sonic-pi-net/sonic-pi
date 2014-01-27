
require 'parslet'

# This is where the famous GobbleUp atom is defined. This parslet atom consumes
# all chars until a given string (absent) is encountered.
#
class GobbleUp < Parslet::Atoms::Base
  def initialize absent, min_chars=0
    @absent = absent
    @min_chars = min_chars
  end

  def try(source, context, consume_all)
    excluding_length = source.chars_until(@absent)

    if excluding_length >= @min_chars
      return succ(source.consume(excluding_length))
    else
      return context.err(self, source, "No such string in input: #{@absent.inspect}.")
    end
  end

  def to_s_inner(prec)
    "until('#{@absent}')"
  end
end