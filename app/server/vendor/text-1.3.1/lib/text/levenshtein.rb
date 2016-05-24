#
# Levenshtein distance algorithm implementation for Ruby, with UTF-8 support.
#
# The Levenshtein distance is a measure of how similar two strings s and t are,
# calculated as the number of deletions/insertions/substitutions needed to
# transform s into t. The greater the distance, the more the strings differ.
#
# The Levenshtein distance is also sometimes referred to as the
# easier-to-pronounce-and-spell 'edit distance'.
#
# Author: Paul Battley (pbattley@gmail.com)
#

module Text # :nodoc:
module Levenshtein

  # Calculate the Levenshtein distance between two strings +str1+ and +str2+.
  #
  # The optional argument max_distance can reduce the number of iterations by
  # stopping if the Levenshtein distance exceeds this value. This increases
  # performance where it is only necessary to compare the distance with a
  # reference value instead of calculating the exact distance.
  #
  # The distance is calculated in terms of Unicode codepoints. Be aware that
  # this algorithm does not perform normalisation: if there is a possibility
  # of different normalised forms being used, normalisation should be performed
  # beforehand.
  #
  def distance(str1, str2, max_distance = nil)
    if max_distance
      distance_with_maximum(str1, str2, max_distance)
    else
      distance_without_maximum(str1, str2)
    end
  end

private
  def distance_with_maximum(str1, str2, max_distance) # :nodoc:
    s = str1.encode(Encoding::UTF_8).unpack("U*")
    t = str2.encode(Encoding::UTF_8).unpack("U*")

    n = s.length
    m = t.length
    big_int = n * m

    # Swap if necessary so that s is always the shorter of the two strings
    s, t, n, m = t, s, m, n if m < n

    # If the length difference is already greater than the max_distance, then
    # there is nothing else to check
    if (n - m).abs >= max_distance
      return max_distance
    end

    return 0 if s == t
    return m if n.zero?
    return n if m.zero?

    # The values necessary for our threshold are written; the ones after must
    # be filled with large integers since the tailing member of the threshold
    # window in the bottom array will run min across them
    d = (m + 1).times.map { |i|
      if i < m || i < max_distance + 1
        i
      else
        big_int
      end
    }
    x = nil
    e = nil

    n.times do |i|
      # Since we're reusing arrays, we need to be sure to wipe the value left
      # of the starting index; we don't have to worry about the value above the
      # ending index as the arrays were initially filled with large integers
      # and we progress to the right
      if e.nil?
        e = i + 1
      else
        e = big_int
      end

      diag_index = t.length - s.length + i

      # If max_distance was specified, we can reduce second loop. So we set
      # up our threshold window.
      # See:
      # Gusfield, Dan (1997). Algorithms on strings, trees, and sequences:
      # computer science and computational biology.
      # Cambridge, UK: Cambridge University Press. ISBN 0-521-58519-8.
      # pp. 263–264.
      min = i - max_distance - 1
      min = 0 if min < 0
      max = i + max_distance
      max = m - 1 if max > m - 1

      min.upto(max) do |j|
        # If the diagonal value is already greater than the max_distance
        # then we can safety return: the diagonal will never go lower again.
        # See: http://www.levenshtein.net/
        if j == diag_index && d[j] >= max_distance
          return max_distance
        end

        cost = s[i] == t[j] ? 0 : 1
        insertion = d[j + 1] + 1
        deletion = e + 1
        substitution = d[j] + cost
        x = insertion < deletion ? insertion : deletion
        x = substitution if substitution < x

        d[j] = e
        e = x
      end
      d[m] = x
    end

    if x > max_distance
      return max_distance
    else
      return x
    end
  end

  def distance_without_maximum(str1, str2) # :nodoc:
    s = str1.encode(Encoding::UTF_8).unpack("U*")
    t = str2.encode(Encoding::UTF_8).unpack("U*")

    n = s.length
    m = t.length

    return m if n.zero?
    return n if m.zero?

    d = (0..m).to_a
    x = nil

    n.times do |i|
      e = i + 1
      m.times do |j|
        cost = s[i] == t[j] ? 0 : 1
        insertion = d[j + 1] + 1
        deletion = e + 1
        substitution = d[j] + cost
        x = insertion < deletion ? insertion : deletion
        x = substitution if substitution < x

        d[j] = e
        e = x
      end
      d[m] = x
    end

    return x
  end

  extend self
end
end
