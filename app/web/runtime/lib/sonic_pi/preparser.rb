# SPDX-License-Identifier: AGPL-3.0-or-later
# native's preparser (app/server/ruby/lib/sonicpi/preparser.rb), as it is: a program's text before it runs. For each
# fn that makes a ring (SonicPi::Data::VEC_FNS, from native's docs), the Lisp-like (ring 1, 2) becomes a call, and
# using the fn's name as a variable (scale = …) is refused, saying which line. Strings and comments are masked
# first, so neither is ever read as code.
module SonicPi
  module PreParser
    class PreParseError < StandardError; end

    STRING_OR_COMMENT = /'(?:\\.|[^'\\])*'|"(?:\\.|[^"\\])*"|#[^\n]*/m

    def self.mask_strings_and_comments(rb)
      rb.gsub(STRING_OR_COMMENT) { |m| m.gsub(/[^\n]/, " ") }
    end

    def self.preparse(rb, vec_fns = SonicPi::Data::VEC_FNS)
      rb = String.new(rb)
      masked = mask_strings_and_comments(rb)
      vec_fns.each do |fn|
        fn = fn.to_s
        re = /\((\s*)#{fn}([,[:space:]]+)/
        pos = 0
        while (m = masked.match(re, pos))
          replacement = " " + m[1] + fn + "(" + (" " * (m[2].size - 1))
          rb[m.begin(0)...m.end(0)] = replacement
          masked[m.begin(0)...m.end(0)] = replacement
          pos = m.end(0)
        end
        if (m = masked.match(/(?!\B)\W?(#{fn})\s*=[\s\w]/))
          line = masked[0...m.begin(1)].count("\n") + 1
          raise PreParseError, "You may not use the built-in fn names as variable names.\n You attempted to use: #{fn} (line #{line})"
        end
      end
      rb
    end
  end
end
