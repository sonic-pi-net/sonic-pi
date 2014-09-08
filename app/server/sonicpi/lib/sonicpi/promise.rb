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
  class Promise

    def initialize
      @prom_sem = Mutex.new
      @value = nil
      @delivered = false
      @received = ConditionVariable.new
    end

    def get(timeout=nil)
      return @value if @delivered
      @prom_sem.synchronize do
        return @value if @delivered
        @received.wait(@prom_sem, timeout)
        if @delivered
          return @value
        else
          raise "Promise timeout"
        end
      end
    end

    def deliver!(val, raise_error=true)
      @prom_sem.synchronize do
        if @delivered
          raise "Promise already delivered. You tried, to deliver #{val.inspect}, however already have: #{@value.inspect}" if raise_error
        else
          @value = val
          @delivered = true
          @received.signal
          val
        end
      end
    end
  end
end
