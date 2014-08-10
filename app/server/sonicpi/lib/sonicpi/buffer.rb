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
module SonicPi
  class Buffer
    attr_reader :id, :num_frames, :num_chans, :sample_rate, :duration
    def initialize(server, id, num_frames, num_chans, sample_rate)
      @server = server
      @id = id
      @num_frames = num_frames
      @num_chans = num_chans
      @sample_rate = sample_rate
      @duration = num_frames.to_f / sample_rate.to_f
      @state = :live
      @mutex = Mutex.new
    end

    def to_i
      @id
    end

    def free
      return false if @state == :killed
      @mutex.synchronize do
        return false if @state == :killed
        @state = :killed
        server.buffer_free(@id)
      end
    end

    def to_s
      "#<Buffer @id=#{@id}, @num_chans=#{@num_chans}, @num_frames=#{num_frames}, @sample_rate=#{@sample_rate}, @duration=#{@duration}>"
    end

    def inspect
      to_s
    end

  end
end
