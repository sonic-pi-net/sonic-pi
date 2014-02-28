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

    def get
      return @value if @delivered
      @val_sem.synchronize do
        return @value if @delivered
        val = @box.pop
        @value = val
        @delivered = true
        val
      end
    end

    def deliver!(val)
      @push_sem.synchronize do
        if(@box.empty? && !@pushed)
          @box.push val
          @pushed = true
        else
          raise "Promise already delivered"
        end
      end
    end
  end
end
