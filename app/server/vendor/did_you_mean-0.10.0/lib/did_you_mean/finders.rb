require "did_you_mean/levenshtein"
require "did_you_mean/jaro_winkler"

module DidYouMean
  module BaseFinder
    AT    = "@".freeze
    EMPTY = "".freeze

    def suggestions
      @suggestions ||= searches.flat_map do |input, candidates|
        input     = normalize(input)
        threshold = input.length > 3 ? 0.834 : 0.77

        seed = candidates.select {|candidate| JaroWinkler.distance(normalize(candidate), input) >= threshold }
          .sort_by! {|candidate| JaroWinkler.distance(candidate.to_s, input) }
          .reverse!

        # Correct mistypes
        threshold   = (input.length * 0.25).ceil
        corrections = seed.select {|c| Levenshtein.distance(normalize(c), input) <= threshold }

        # Correct misspells
        if corrections.empty?
          corrections = seed.select do |candidate|
            candidate = normalize(candidate)
            length    = input.length < candidate.length ? input.length : candidate.length

            Levenshtein.distance(candidate, input) < length
          end.first(1)
        end

        corrections
      end
    end

    def searches
      raise NotImplementedError
    end

    private

    def normalize(str_or_symbol) #:nodoc:
      str = if str_or_symbol.is_a?(String)
              str_or_symbol.dup
            else
              str_or_symbol.to_s
            end

      str.downcase!
      str.tr!(AT, EMPTY)
      str
    end
  end

  class NullFinder
    def initialize(*);  end
    def suggestions; [] end
  end
end

require 'did_you_mean/finders/name_error_finders'
require 'did_you_mean/finders/method_finder'
