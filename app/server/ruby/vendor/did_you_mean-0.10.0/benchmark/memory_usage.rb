require 'memory_profiler'
require 'did_you_mean'

# public def foo; end
# error      = (self.fooo rescue $!)
# executable = -> { error.to_s }

class DidYouMean::WordCollection
  include DidYouMean::BaseFinder

  def initialize(words)
    @words = words
  end

  def similar_to(input, filter = EMPTY)
    @suggestions, @input = nil, input
    suggestions
  end

  def searches
    { @input => @words }
  end
end if !defined?(DidYouMean::WordCollection)

METHODS    = ''.methods
INPUT      = 'start_with?'
collection = DidYouMean::WordCollection.new(METHODS)
executable = proc { collection.similar_to(INPUT) }

GC.disable
MemoryProfiler.report { 100.times(&executable) }.pretty_print
