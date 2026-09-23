# SPDX-License-Identifier: AGPL-3.0-or-later
# Note names to MIDI numbers, as Sonic Pi's Note does: a letter, an optional
# sharp or flat (s, f or b, any case), an optional octave (default 4), so
# :c4 is 60. The name is walked by hand.
module SonicPi
  module Note
    INTERVALS = { "c" => 0, "d" => 2, "e" => 4, "f" => 5, "g" => 7, "a" => 9, "b" => 11 }
    DEFAULT_OCTAVE = 4

    def self.midi(n)
      case n
      when Numeric then return n
      when Symbol, String then nil
      else raise InvalidNoteError, "Invalid note: #{n.inspect}"
      end
      s = n.to_s
      s = s[1..] if s.start_with?(":")
      i = 0
      letter = s[0].to_s.downcase
      interval = INTERVALS[letter] or raise InvalidNoteError, "Invalid note: #{n.inspect}"
      i += 1
      mod = s[i].to_s.downcase
      if mod == "s"
        interval += 1
        i += 1
      elsif mod == "f" || mod == "b"
        interval -= 1
        i += 1
      end
      rest = s[i..].to_s
      octave = DEFAULT_OCTAVE
      unless rest.empty?
        digits = rest.start_with?("-") ? rest[1..] : rest
        raise InvalidNoteError, "Invalid note: #{n.inspect}" if digits.empty? || digits.each_char.any? { |c| c < "0" || c > "9" }
        octave = rest.to_i
      end
      (octave * 12) + interval + 12
    end

    def self.rest?(n)
      n.nil? || n == :r || n == :rest
    end
  end
end

# A note name does arithmetic as its MIDI number, as Sonic Pi's western_theory.rb has it: play :e3 + 4 is 56, and
# :e4 - :c4 is 4. A rest stays a rest.
class Symbol
  def +(other)
    return self if self == :r || self == :rest
    SonicPi::Note.midi(self) + SonicPi::Note.midi(other)
  end

  def -(other)
    return self if self == :r || self == :rest
    SonicPi::Note.midi(self) - SonicPi::Note.midi(other)
  end

  def to_f
    return 0.0 if self == :r || self == :rest
    SonicPi::Note.midi(self).to_f
  end

  def to_i = to_f.to_i
end

# A rest does no arithmetic: nil plus or minus anything is nil, as native's western_theory.rb has it, so a rest in a
# list of notes stays a rest through a transposition (play [:c4, :r, :g4], (ring 60, nil).tick + 12).
class NilClass
  def +(other) = nil
  def -(other) = nil
end

# A Float counts as native's core.rb has it: to_i times, each count a Float (density 0.5, reps: 2.0).
class Float
  def times
    to_i.times { |i| yield i.to_f }
  end
end

# Range#step, which Ruby has and mruby does not: each value begin + i * step (so a Float step does not drift), up
# to the end or short of it for a ... range. Without a block, the values, as an Enumerator gives them to each and to_a.
unless Range.method_defined?(:step)
  class Range
    def step(n = 1, &blk)
      raise ArgumentError, "step can't be 0" if n == 0
      raise ArgumentError, "step can't be negative" if n < 0
      vals = []
      i = 0
      loop do
        x = self.begin + i * n
        break if exclude_end? ? x >= self.end : x > self.end
        vals << x
        i += 1
      end
      return (vals.each(&blk); self) if blk
      vals
    end
  end
end
