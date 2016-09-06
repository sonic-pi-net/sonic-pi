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
  class BufferStream < Buffer
    attr_reader :buffer, :path, :size, :extension, :sample_format, :start_frame, :leave_open
    def initialize(server, buffer, path, size, num_chans, extension, sample_format, num_frames, start_frame, leave_open)

      @num_frames = num_frames
      @num_chans = num_chans

      @server = server
      @buffer = buffer
      @path = path
      @size = size

      @extension = extension
      @sample_format = sample_format

      @start_frame = start_frame
      @leave_open = leave_open
      @state = :live
      @mutex = Mutex.new
    end

    def close
      return false if @state == :killed
      @mutex.synchronize do
        return false if @state == :killed
        @state = :killed
        @server.buffer_stream_close(@buffer)
      end
    end

    def free
      close
    end

    def to_i
      @buffer.id
    end

    def duration
      raise "Open BufferStream - duration is unknown" if @leave_open
      @buffer.duration
    end

    def sample_rate
      @buffer.sample_rate
    end

    def to_s
      "#<BufferStream @buffer=#{@buffer.id}, @path=#{@path}, @size=#{@size}, @extension=#{@extension}, @sample_format=#{@sample_format}, @num_chans=#{@num_chans}>"
    end
  end
end
