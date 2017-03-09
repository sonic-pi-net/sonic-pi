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

module SonicPi
  class State
    def initialize(memory_time = 10)
      # memory_time is in seconds
      @memory_time = memory_time * 1000
      reset!
      @state_mut = Mutex.new
    end

    def get(k, t, default=nil)
      t = t.to_f
      wait_for_threads!(t)
      vals = nil
      @state_mut.synchronize do
        vals = @state[k]
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
      t = t.to_f
      current_time = current_time.to_f
      @state_mut.synchronize do
        tvals = @state[k]
        tvals.push [t, v]
        tvals.sort! {|a, b| a[0] <=> b[0]}

        # remove values which are older than @memory_time seconds ago

        idx = tvals.find_index {|tv| tv[0] >= (current_time - @memory_time)}

        idx = tvals.size - 1 if !idx && tvals.size > 0

        tvals.shift(idx) if idx
        @state[k] = tvals
      end
    end

    def reset!
      @state = Hash.new([])
    end

    def to_s
      @state.to_s
    end

    def ispect
      @state.inspect
    end


    private

    def wait_for_threads!(t)
      # implement me!
    end
  end
end
