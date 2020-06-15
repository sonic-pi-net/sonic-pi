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
require_relative "bus"
require_relative "allocator"

module SonicPi
  class BusAllocator
    def initialize(max_bus_id, idx_offset=0)
      # allocate busses in pairs
      @allocation_size = allocation_size
      @idx_offset = idx_offset
      @max_id = ((max_bus_id - idx_offset) / allocation_size) - 1
      @allocator = Allocator.new(@max_id)
    end

    def allocation_size
      1
    end

    def allocate
      new_id = (@allocator.allocate * allocation_size) + @idx_offset
      bus_class.new(new_id, self)
    end

    def release!(id)
      idx = (id - @idx_offset) / allocation_size
      @allocator.release! idx
    end

    def reset!
      @allocator.reset!
    end

    def num_busses_allocated
      @allocator.num_allocations
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
