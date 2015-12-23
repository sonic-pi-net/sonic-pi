#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
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
      reset!
    end

    def allocate
      @available.pop
    end

    def release! idx
      #This implementation doesn't catch multiple similar
      #releases polluting the queue with duplications
      @available << idx
    end

    def reset!
      new_queue = Queue.new
      (0..@max_id).each do |el|
        new_queue << el
      end
      @available = new_queue
    end

    def num_allocations
      (@max_id - @available.size) + 1
    end

    def to_s
      "<#SonicPi::Allocator>"
    end

    def inspect
      to_s
    end
  end
end
