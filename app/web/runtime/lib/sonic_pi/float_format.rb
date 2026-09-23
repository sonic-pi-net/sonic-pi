# SPDX-License-Identifier: AGPL-3.0-or-later
# Ruby prints a Float as the shortest decimal that reads back as the same
# number, in exponent form below 1e-4 and from 1e16. mruby 4.1 finds the same
# digits but goes to exponent form from 1e15 (1794244848942944.5 prints as
# 1.7942448489429445e+15). Programs see floats through puts, so the runtime
# formats them Ruby's way.

# Float#round(digits) as CRuby does it (numeric.c rb_float_round and
# round_half_up), not as mruby does. CRuby rounds x * 10**digits, then nudges
# the result up when (f + 0.5) / 10**digits is still <= x, and near 2**52 the
# + 0.5 rounds away, so 46.0.round(14) is 46.00000000000001 there and 46.0 in
# mruby. range rounds every step to 14 digits, so line, and every ring built
# from it, carries that nudge in Sonic Pi.
if Object.const_defined?(:RUBY_ENGINE) && RUBY_ENGINE == "mruby"
  class Float
    alias_method :__mruby_round, :round

    def round(ndigits = 0, *rest)
      return __mruby_round(ndigits, *rest) unless rest.empty? && ndigits.is_a?(Integer) && ndigits > 0 && ndigits <= 14 && finite?
      return self if zero?
      binexp = Math.frexp(self)[1]
      trunc = ->(a, b) { (a.to_f / b).truncate }                       # C's integer division
      return self if ndigits >= 17 - (binexp > 0 ? trunc.(binexp, 4) : trunc.(binexp, 3) - 1)   # float_round_overflow
      return 0.0 if ndigits < -(binexp > 0 ? trunc.(binexp, 3) + 1 : trunc.(binexp, 4))       # float_round_underflow
      s = (10**ndigits).to_f
      f = (self * s).__mruby_round.to_f
      if self > 0
        f += 1 if (f + 0.5) / s <= self
      elsif (f - 0.5) / s >= self
        f -= 1
      end
      f / s
    end
  end
end

module SonicPi
  module FloatFormat
    # Under CRuby this is Float#to_s itself; under our mruby it is the C
    # helper in mrbgems/sonic-pi-core, which matches CRuby on 400,000 values
    # where mruby's own to_s lays out one in forty differently. The Ruby
    # version remains for any mruby without the gem, and needs one whose
    # String#to_f is correctly rounded (4.1's is).
    def self.to_s(f)
      return SonicPi::Native.float_to_s(f) if SonicPi.const_defined?(:Native)
      return f.to_s if Object.const_defined?(:RUBY_ENGINE) && RUBY_ENGINE == "ruby"
      pure_to_s(f)
    end

    def self.pure_to_s(f)
      return "NaN" if f.nan?
      return (f > 0 ? "Infinity" : "-Infinity") if f.infinite?
      return (1.0 / f < 0 ? "-0.0" : "0.0") if f == 0.0
      digits = nil
      exp = nil
      (1..17).each do |p|
        s = format("%.#{p - 1}e", f)      # d.ddddde±xx
        if s.to_f == f
          m, e = s.split("e")
          digits = m.delete("-").delete(".")
          exp = e.to_i
          break
        end
      end
      digits = digits[0, digits.size - 1] while digits.size > 1 && digits.end_with?("0")   # by hand, not a Regexp
      sign = f < 0 ? "-" : ""
      # Ruby's thresholds: exponent form below 1e-4, from 1e16, and for a
      # 16-digit integer part with nothing after the point.
      if exp < -4 || exp >= 16 || (exp == 15 && digits.size <= 16)
        mant = digits.size > 1 ? "#{digits[0]}.#{digits[1..]}" : "#{digits}.0"
        "#{sign}#{mant}e#{exp < 0 ? '-' : '+'}#{format('%02d', exp.abs)}"
      elsif exp < 0
        "#{sign}0.#{'0' * (-exp - 1)}#{digits}"
      elsif digits.size <= exp + 1
        "#{sign}#{digits}#{'0' * (exp + 1 - digits.size)}.0"
      else
        "#{sign}#{digits[0..exp]}.#{digits[(exp + 1)..]}"
      end
    end
  end
end
