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

module SonicPi
  #TODO: Deprecate in favour of Core::RingArray
  class WrappingArray < Array
    def [](idx, len=nil)
      return self.to_a[idx, len] if len

      idx = idx.to_i % size if idx.is_a? Numeric
      self.to_a[idx]
    end

    def slice(idx, len=nil)
      return self.to_a.slice(idx, len) if len

      idx = idx.to_i % size if idx.is_a? Numeric
      self.to_a.slice(idx)
    end
  end
end
