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
      @val_sem = Mutex.new
      @push_sem = Mutex.new
      @box = Queue.new
      @value = nil
      @delivered = false
      @pushed = false
    end

    def get_with_timeout(timeout, resolution)
      raise "Promise timeout resolution must be positive" if resolution <= 0
      raise "Promise timeout must be positive" if timeout <= 0
      return @value if @delivered
      value_obtained = false
      val = nil
      @val_sem.synchronize do
        return @value if @delivered
        begin
          #use non-blocking Queue#pop which raises an exception if value
          #isn't available:
          val = @box.pop(true)
          @value = val
          @delivered = true
          value_obtained = true
        rescue
          # no value in box
          val = nil
          value_obtained = false
        end
      end

      if value_obtained
        return val
      else
        new_time = timeout - resolution
        if new_time > 0
          sleep resolution
          get_with_timeout(timeout - resolution, resolution)
        else
          raise "Timeout attempting to get value from promise #{self}"
        end
      end
    end

    def get()
      return @value if @delivered
      @val_sem.synchronize do
        return @value if @delivered
        val = @box.pop
        @value = val
        @delivered = true
        val
      end
    end

    def deliver!(val, raise_error=true)
      @push_sem.synchronize do
        if @pushed
          raise "Promise already delivered. You tried, to deliver #{val.inspect}, however already have: #{@incoming_val_for_error.inspect}" if raise_error
        else
          @incoming_val_for_error = val
          @box.push val
          @pushed = true
        end
      end
    end
  end
end
