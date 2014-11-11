module DidYouMean
  module Levenshtein
    # This code is based directly on the Text gem implementation
    # Returns a value representing the "cost" of transforming str1 into str2
    def distance(str1, str2)
      n = str1.length
      m = str2.length
      return m if n.zero?
      return n if m.zero?

      d = (0..m).to_a
      x = nil

      str1.each_char.with_index(1) do |char1, i|
        str2.each_char.with_index do |char2, j|
          cost = (char1 == char2) ? 0 : 1
          x = min3(
            d[j+1] + 1, # insertion
            i + 1,      # deletion
            d[j] + cost # substitution
          )
          d[j] = i
          i = x
        end
        d[m] = x
      end

      x
    end
    module_function :distance

    private

    def min3(a, b, c) # :nodoc:
      if a < b && a < c
        a
      elsif b < c
        b
      else
        c
      end
    end
    module_function :min3
  end
end
