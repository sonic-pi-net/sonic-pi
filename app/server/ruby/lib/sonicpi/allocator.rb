#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

module SonicPi
  class AllocationError < Exception ; end
  class Allocator
    attr_reader :max_id
    def initialize(max_id)
      @max_id = max_id
      @mut = Mutex.new
      @last_used_idx = 0
      reset!
    end

    def allocate
      @mut.synchronize do
        attempts = 0
        while attempts < @max_id
          @last_used_idx = (@last_used_idx + 1) % @max_id
          if @allocations[@last_used_idx] == false
            @allocations[@last_used_idx] = true
            return @last_used_idx
          end
          attempts += 1
        end
      end
      raise AllocationError
    end

    def release! idx
      @mut.synchronize do
        @allocations[idx] = false
      end
    end

    def reset!
      @mut.synchronize do
        @allocations = [false] * @max_id
      end
    end

    def num_allocations
      @allocations.reduce(0) do |s, v|
        s += 1 if v
        s
      end
    end

    def to_s
      "<#SonicPi::Allocator>"
    end

    def inspect
      to_s
    end
  end
end
