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
  class SampleBuffer < Buffer
    def initialize(buffer, path)
      @buffer = buffer
      @path = path
    end

    def num_frames
      @buffer.num_frames
    end

    def num_chans
      @buffer.num_chans
    end

    def sample_rate
      @buffer.sample_rate
    end

    def duration
      @buffer.duration
    end

    def buffer
      @buffer
    end

    def state
      @buffer.state
    end

    def path
      @path
    end

    def free
      @buffer.free
    end

    def id
      @buffer.id
    end

    def to_i
      @buffer.to_i
    end

    def inspect
      to_s
    end

    def to_s
      if @buffer.path
        "#<SampleBuffer @id=#{@buffer.id}, @num_chans=#{@buffer.num_chans}, @num_frames=#{@buffer.num_frames}, @sample_rate=#{@buffer.sample_rate}, @duration=#{@buffer.duration}, @path=#{@buffer.path}>"
      else
        "#<SampleBuffer @id=#{@buffer.id}, @num_chans=#{@buffer.num_chans.inspect}, @num_frames=#{@buffer.num_frames}, @sample_rate=#{@buffer.sample_rate}, @duration=#{@buffer.duration}>"
      end
    end
  end
end
