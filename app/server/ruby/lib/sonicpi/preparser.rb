#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "lang/support/docsystem"

module SonicPi
  module PreParser

    class PreParseError < StandardError ; end

    STRING_OR_COMMENT = /'(?:\\.|[^'\\])*'|"(?:\\.|[^"\\])*"|#[^\n]*/m

    # String literals and comments are opaque to the preparser: matching
    # happens against a masked copy (contents blanked, newlines kept) so
    # positions line up, and the length-preserving ring transform is
    # applied to the real source by position.
    def self.mask_strings_and_comments(rb)
      rb.gsub(STRING_OR_COMMENT) { |m| m.gsub(/[^\n]/, " ") }
    end

    def self.preparse(rb, vec_fns)
      rb = String.new(rb)
      masked = mask_strings_and_comments(rb)
      vec_fns.each do |fn|
        fn = fn[:name].to_s
        re = /\((\s*)#{fn}([,[:space:]]+)/
        pos = 0
        while (m = masked.match(re, pos))
          replacement = ' ' + m[1] + fn + '(' + (' ' * (m[2].size - 1))
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
