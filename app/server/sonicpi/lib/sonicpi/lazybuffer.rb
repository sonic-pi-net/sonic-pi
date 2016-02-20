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
      return @num_frames if @realised
      wait_for_prom
      @num_frames
    end

    def num_chans
      return @num_chans if @realised
      wait_for_prom
      @num_chans
    end

    def sample_rate
      return @sample_rate if @realised
      wait_for_prom
      @sample_rate
    end

    def duration
      return @duration if @realised
      wait_for_prom
      @duration
    end

    private

    def wait_for_prom
      return true if @realised
      @prom_mut.synchronize do
        return true if @realised
        num_frames, num_chans, sample_rate = @prom.get
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
