
require 'stringio'
require 'strscan'

require 'parslet/source/line_cache'

module Parslet
  # Wraps the input string for parslet. 
  #
  class Source
    def initialize(str)
      raise(
        ArgumentError, 
        "Must construct Source with a string like object."
      ) unless str.respond_to?(:to_str)

      @str = StringScanner.new(str)

      # maps 1 => /./m, 2 => /../m, etc...
      @re_cache = Hash.new { |h,k| 
        h[k] = /(.|$){#{k}}/m }

      @line_cache = LineCache.new
      @line_cache.scan_for_line_endings(0, str)
    end
  
    # Checks if the given pattern matches at the current input position. 
    #
    # @param pattern [Regexp] pattern to check for
    # @return [Boolean] true if the pattern matches at #pos
    #
    def matches?(pattern)
      @str.match?(pattern)
    end
    alias match matches?
    
    # Consumes n characters from the input, returning them as a slice of the
    # input. 
    #
    def consume(n)
      original_pos = @str.pos
      slice_str = @str.scan(@re_cache[n])
      slice = Parslet::Slice.new(
        slice_str,
        original_pos,
        @line_cache)

      return slice
    end
    
    # Returns how many chars remain in the input. 
    #
    def chars_left
      @str.rest_size
    end

    # Returns how many chars there are between current position and the 
    # string given. If the string given doesn't occur in the source, then 
    # the remaining chars (#chars_left) are returned. 
    #
    # @return [Fixnum] count of chars until str or #chars_left
    #
    def chars_until str
      slice_str = @str.check_until(Regexp.new(Regexp.escape(str)))
      return chars_left unless slice_str
      return slice_str.size - str.size
    end
    
    # Position of the parse as a character offset into the original string. 
    # @note: Encodings...
    def pos
      @str.pos
    end
    def pos=(n)
      @str.pos = n
    rescue RangeError
    end

    # Returns a <line, column> tuple for the given position. If no position is
    # given, line/column information is returned for the current position
    # given by #pos. 
    #
    def line_and_column(position=nil)
      @line_cache.line_and_column(position || self.pos)
    end
  end
end
