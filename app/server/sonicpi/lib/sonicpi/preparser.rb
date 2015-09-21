#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "lang/core"
require_relative "lang/sound"
require_relative "lang/support/docsystem"

module SonicPi
  module PreParser

    class PreParseError < StandardError ; end

    def self.preparse(rb)
      SonicPi::Lang::Core.vec_fns.each do |fn|
        fn = fn[:name].to_s
        rb.gsub!(/\((\s*)#{fn}(\s)/, '\1' + fn + '(\2')

        if rb.match(/(?!\B)\W?#{fn}\s*=[\s\w]/)
          raise PreParseError, "You may not use the built-in fn names as variable names.\n You attempted to use: #{fn}"
        end
      end
      rb
    end
  end
end
