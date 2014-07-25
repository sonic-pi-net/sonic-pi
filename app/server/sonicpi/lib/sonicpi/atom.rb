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

## Assumes Immutable values used as val...
module SonicPi
  class Atom
    def initialize(val)
      @val = val
      @sem = Mutex.new
    end

    def swap!(&block)
      Thread.current.thread_variable_set :sonic_pi_atom_last, @val
      Thread.current.thread_variable_set :sonic_pi_atom_new, block.call(@val)
      @sem.synchronize do
        last = Thread.current.thread_variable_get(:sonic_pi_atom_last)
        new = Thread.current.thread_variable_get(:sonic_pi_atom_new)
        if @val == last
          @val = new
          return new
        end
      end

      ## Didn't work, try again...
      swap!(&block)
    end

    def swap_returning_old!(&block)
      Thread.current.thread_variable_set :sonic_pi_atom_last, @val
      Thread.current.thread_variable_set :sonic_pi_atom_new, block.call(@val)
      @sem.synchronize do
        last = Thread.current.thread_variable_get(:sonic_pi_atom_last)
        new = Thread.current.thread_variable_get(:sonic_pi_atom_new)
        if @val == last
          @val = new
          return last
        end
      end

      ## Didn't work, try again...
      swap_returning_old!(&block)
    end

    def deref
      @val
    end

    def reset!(new_val)
      @sem.synchronize do
        @val = new_val
      end
    end
  end
end
