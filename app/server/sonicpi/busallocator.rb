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
require_relative "bus"
require_relative "atom"
require_relative "allocator"
require "hamster/vector"

module SonicPi
  class BusAllocator < Allocator
    def initialize(max_bus_id, idx_offset=0)
      super
      @busses_A = Atom.new(Hamster.vector)
    end

    def allocate(num_adj_busses)
      new_id = super(num_adj_busses)
      bus = bus_class.new(new_id, num_adj_busses, self){|id, size| release!(id-self.idx_offset, size)}

      @busses_A.swap! {|bs| bs.add(bus)}
      bus
    end

    def release(bus)
      bus.free
    end

    def reset!
      super
      old_busses = @busses_A.swap_returning_old! {|bs| Hamster.vector}
      old_busses.each {|b| b.free}
      @busses_A.reset! Hamster.vector
    end

    def num_busses_allocated
      num_allocations
    end

    def to_s
      "<#SonicPi::BusAllocator>"
    end

    private

    def bus_class
      raise "Implement me!"
    end
  end
end
