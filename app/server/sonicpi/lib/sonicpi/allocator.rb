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
require_relative "atom"
require "hamster/vector"

module SonicPi
  class AllocationError < Exception ; end
  class Allocator
    attr_reader :max_id, :idx_offset
    def initialize(max_id, idx_offset=0)
      @idx_offset = idx_offset
      @max_id = max_id
      tmp_ids = [false] * @max_id
      @ids_A = Atom.new(Hamster.vector(*tmp_ids))
      @num_allocations = 0
    end

    def allocate(num_adj_ids=1)
      idx = nil
      ids = @ids_A.swap! do |ids|
        idx = find_gap(0, num_adj_ids, ids)
        new_ids = (idx...idx+num_adj_ids).reduce(ids) {|is, i| is.set(i, true)}
      end
      new_id = idx + @idx_offset
      @num_allocations += num_adj_ids
      new_id
    end

    def release!(idx, num_adj_ids=1)
      @ids_A.swap! do |ids|
        (idx...idx+num_adj_ids).reduce(ids) {|is, i| is.set(i, false)}
      end
      @num_allocations -= num_adj_ids
    end

    def reset!
      old_ids = @ids_A.swap_returning_old! {|bs| Hamster.vector}
      old_ids.each {|b| b.free}
      @num_busses_allocated = 0
      @busses_A.reset! Hamster.vector
    end

    def num_allocations
      @num_allocations
    end

    def to_s
      "<#SonicPi::Allocator>"
    end

    def inspect
      to_s
    end

    private

    def valid_gap?(idx, gap_size, ids)
      seg = ids.to_a[idx...idx+gap_size]
      seg.all?{|el| !el}
    end

    def find_gap(idx, gap_size, ids)
      while idx < @max_id
        if valid_gap?(idx, gap_size, ids)
          return idx
        else
          idx += 1
        end
      end

      raise AllocationError, "Unable to allocate bus"
    end
  end
end
