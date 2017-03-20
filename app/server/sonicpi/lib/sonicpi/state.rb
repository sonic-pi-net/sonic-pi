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
      @memory_time = (opts.fetch :memory_time, 10)
      @multi_write = opts.fetch :multi_write, true
      @state_mut = Mutex.new
      reset!
    end

    def get(t, k, default=nil)
      get_tbv(t, k, default)[2]
    end

    def get_tbv(t, k, default=nil)
      # Get immediate previous state item  *at* or *before* time t
      t = t.to_f
      wait_for_threads!(t) if @multi_write
      vals = nil
      @state_mut.synchronize do
        vals = @state.fetch(k, []).clone
      end

      res = [-1, 0, default]
      vals.each do |tv|
        # if this time/val pair has a timestamp
        # after t, then it's not for us - return
        # last seen val
        return res if tv[0] > t
        res =  tv
      end

      return res
    end


    def wait_next_tbv(t, k, &blk)
      # Get immediate next state item *after* time t
      # will block thread if necessary
      # returns a list of [time, beat, value]
      t = t.to_f
      prom = nil
      @state_mut.synchronize do
        vals = @state.fetch(k, [])
        idx = vals.size - 1
        res = [-1, 0, nil]

        if !vals[idx] || (vals[idx][0] <= t)
          # register an event
          # leave the mutex and then wait

          prom = Promise.new
          w = @waiters[k]
          if w
            w << [t, prom]
            w.sort! {|a, b| a[0] <=> b[0]}
          else
            @waiters[k] = [[t, prom]]
          end
        else
          while idx >= 0
            v = vals[idx]
            if v[0] > t
              res = v
            else
              return res
            end
            idx -= 1
          end
        end
      end

      # wait for new set to come in from
      # another thread which will be after t
      blk.call if blk
      if @multi_write
        prom.get
        wait_for_threads!(t)
        return get(t, k)
      else
        return prom.get
      end

    end

    def set(t, b, k, v, now = Time.now)
      t = t.to_f
      now = now.to_f
      @state_mut.synchronize do
        tvals = @state.fetch(k, [])
        tvals.push [t, b, v]
        # ensure list is sorted if in multi_write mode
        # if not, we can assume it is always sorted as there
        # will only ever be one producer writing in events
        # with its own single timeline
        tvals.sort! {|x, y| x[0] <=> y[0]} if @multi_write

        if @update_count[k] > 50
          # remove values which are older than @memory_time seconds ago
          idx = tvals.find_index {|tv| tv[0] >= (now - @memory_time)}
          idx = tvals.size - 1 if !idx && tvals.size > 0
          tvals.shift(idx) if idx
          @update_count[k] = 0
        end
        @state[k] = tvals
        @update_count[k] += 1

        waiters = @waiters[k]
        if waiters
          while waiters[0] && waiters[0][0] < t
            waiters[0][1].deliver! [t, b, v]
            waiters.shift
          end
          @waiters.delete(k) if waiters.size == 0
        end
      end
      nil
    end

    def reset!
      @state = {}
      @update_count = Hash.new(0)
      @waiters = {}
    end

    def to_s
      @state.to_s
    end

    def inspect
      @state.inspect
    end

    def keys
      @state.keys
    end

    private

    def wait_for_threads!(t)
      # implement me properly!
      Kernel.sleep(0.01)
    end
  end
end
