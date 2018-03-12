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

require_relative "buffer"
module SonicPi
  class LazyBuffer < Buffer
    def initialize(server, id, prom)
      @server = server
      @id = id
      @prom = prom
      @realised = false
      @prom_mut = Mutex.new
    end

    def num_frames
      wait_for_prom unless @realised
      @num_frames
    end

    def num_chans
      wait_for_prom unless @realised
      @num_chans
    end

    def sample_rate
      wait_for_prom unless @realised
      @sample_rate
    end

    def duration
      wait_for_prom unless @realised
      @duration
    end

    def to_s
      wait_for_prom unless @realised
      if @path
        "#<Buffer @id=#{@id}, @num_chans=#{@num_chans}, @num_frames=#{@num_frames}, @sample_rate=#{@sample_rate}, @duration=#{@duration}, @path=#{@path}>"
      else
        "#<Buffer @id=#{@id}, @num_chans=#{@num_chans.inspect}, @num_frames=#{@num_frames}, @sample_rate=#{@sample_rate}, @duration=#{@duration}>"
      end
    end

    def wait_for_allocation
      wait_for_prom
    end


    private

    def wait_for_prom
      return true if @realised
      @prom_mut.synchronize do
        return true if @realised
        res = @prom.get
        if res.is_a? Exception
          raise res
        else
          num_frames, num_chans, sample_rate = res
        end
        @num_frames = num_frames
        @num_chans = num_chans
        @sample_rate = sample_rate
        @duration = num_frames.to_f / sample_rate.to_f
        @state = :live
        @mutex = Mutex.new
        @path = nil
        @realised = true
      end
    end
  end
end
