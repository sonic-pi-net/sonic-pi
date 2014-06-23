#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, distribution,
# and distribution of modified versions of this work as long as this
# notice is included.
#++
require 'thread'

module SonicPi
  class Counter
    def initialize(init_val=0)
      @init_val = init_val
      @val = init_val
      @mut = Mutex.new
    end

    def next
      @mut.synchronize do
        @val += 1
      end
    end

    def reset!
      @mut.synchronize do
        @val = @init_val
      end
    end
  end
end
