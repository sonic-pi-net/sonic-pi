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
module SonicPi
  class Bus
    attr_reader :id
    def initialize(id, allocator)
      @id = id
      @allocator = allocator
      @live = true
      @sem = Mutex.new
    end

    def rate
      raise "Unknown rate - inherit and implement me!"
    end

    def free
      @sem.synchronize do
        if @live
          @allocator.release!(@id)
          @live = false
          true
        else
          false
        end
      end
    end

    def live?
      @live
    end

    def to_i
      @id
    end

    def to_f
      @id.to_f
    end

    def sp_thread_safe?
      true
    end

    def to_s
      "#<SonicPi::Bus @id=#{@id}>"
    end

    def inspect
      to_s
    end
  end
end
