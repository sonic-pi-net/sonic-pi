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
require 'thread'

## Note: this promise implementation is modelled on the semantics of
## Clojure's promise.  See: https://clojuredocs.org/clojure.core/promise

module SonicPi
  class PromiseTimeoutError < StandardError ; end
  class PromiseAlreadyDeliveredError < StandardError ; end

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
          raise PromiseTimeoutError, "Promise timed out after #{timeout} seconds."
        end
      end
    end

    def deliver!(val, raise_error=true)
      @prom_sem.synchronize do
        if @delivered
          raise PromiseAlreadyDeliveredError, "Promise already delivered. You tried, to deliver #{val.inspect}, however already have: #{@value.inspect}" if raise_error
        else
          @value = val
          @delivered = true
          @received.broadcast
          val
        end
      end
    end

    def delivered?
      @delivered
    end

    def to_s
      "<Promise delivered: #{@delivered}>"
    end
  end
end
