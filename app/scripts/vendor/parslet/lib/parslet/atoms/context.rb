module Parslet::Atoms
  # Helper class that implements a transient cache that maps position and
  # parslet object to results. This is used for memoization in the packrat
  # style. 
  #
  # Also, error reporter is stored here and error reporting happens through
  # this class. This makes the reporting pluggable. 
  #
  class Context
    # @param reporter [#err, #err_at] Error reporter (leave empty for default 
    #   reporter)
    def initialize(reporter=Parslet::ErrorReporter::Tree.new)
      @cache = Hash.new { |h, k| h[k] = {} }
      @reporter = reporter
      @captures = Parslet::Scope.new
    end
    
    # Caches a parse answer for obj at source.pos. Applying the same parslet
    # at one position of input always yields the same result, unless the input
    # has changed. 
    #
    # We need the entire source here so we can ask for how many characters 
    # were consumed by a successful parse. Imitation of such a parse must 
    # advance the input pos by the same amount of bytes.
    #
    def try_with_cache(obj, source, consume_all)
      beg = source.pos
        
      # Not in cache yet? Return early.
      unless entry = lookup(obj, beg)
        result = obj.try(source, self, consume_all)
    
        if obj.cached?
          set obj, beg, [result, source.pos-beg]
        end
        
        return result
      end

      # the condition in unless has returned true, so entry is not nil.
      result, advance = entry

      # The data we're skipping here has been read before. (since it is in 
      # the cache) PLUS the actual contents are not interesting anymore since
      # we know obj matches at beg. So skip reading.
      source.pos = beg + advance
      return result
    end  
    
    # Report an error at a given position. 
    # @see ErrorReporter
    #
    def err_at(*args)
      return [false, @reporter.err_at(*args)] if @reporter
      return [false, nil]
    end
    
    # Report an error. 
    # @see ErrorReporter
    #
    def err(*args)
      return [false, @reporter.err(*args)] if @reporter
      return [false, nil]
    end
  
    # Returns the current captures made on the input (see
    # Parslet::Atoms::Base#capture). Use as follows: 
    # 
    #   context.captures[:foobar] # => returns capture :foobar
    #
    attr_reader :captures
    
    # Starts a new scope. Use the #scope method of Parslet::Atoms::DSL
    # to call this. 
    #
    def scope
      captures.push
      yield
    ensure
      captures.pop
    end
    
  private 
    def lookup(obj, pos)
      @cache[pos][obj] 
    end
    def set(obj, pos, val)
      @cache[pos][obj] = val
    end
  end
end