#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2017 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++
require_relative "util"

module SonicPi
  class State

    include SonicPi::Util

    def initialize(opts={})
      # memory_time is in seconds
      @memory_time = (opts.fetch :memory_time, 10) * 1000
      @multi_write = opts.fetch :multi_write, true
      @state_mut = Mutex.new
      reset!
    end

    def get(k, t, default=nil)
      t = t.to_f
      wait_for_threads!(t) if @multi_write
      vals = nil
      @state_mut.synchronize do
        vals = @state.fetch(k, [])
      end

      res = default
      vals.each do |tv|
        # if this time/val pair has a timestamp
        # after t, then it's not for us - return
        # last seen val
       return res if tv[0] > t
        res = tv[1]
      end

      return res
    end

    def set(k, t, v, current_time = Time.now)
      @state_mut.synchronize do
        t = t.to_f
        current_time = current_time.to_f
        tvals = @state.fetch(k, [])
        tvals.push [t, v]

        # ensure list is sorted if in multi_write mode
        # if not, we can assume it is always sorted as there
        # will only ever be one producer writing in events
        # with its own single timeline
        tvals.sort! {|a, b| a[0] <=> b[0]} if @multi_write

        # remove values which are older than @memory_time seconds ago

        idx = tvals.find_index {|tv| tv[0] >= (current_time - @memory_time)}

        idx = tvals.size - 1 if !idx && tvals.size > 0

        tvals.shift(idx) if idx
        @state[k] = tvals
      end
      nil
    end

    def reset!
      @state = {}
    end

    def to_s
      @state.to_s
    end

    def ispect
      @state.inspect
    end


    private

    def wait_for_threads!(t)
      # implement me properly!
      Kernel.sleep(0.01)
    end
  end
end
