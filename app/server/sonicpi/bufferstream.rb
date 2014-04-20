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
require_relative "buffer"

module SonicPi
  class BufferStream < Buffer
    attr_reader :buffer, :path, :size, :n_chans, :extension, :sample_format, :n_frames, :start_frame, :leave_open
    def initialize(server, buffer, path, size, n_chans, extension, sample_format, n_frames, start_frame, leave_open)
      @server = server
      @buffer = buffer
      @path = path
      @size = size
      @n_chans = n_chans
      @extension = extension
      @sample_format = sample_format
      @n_frames = n_frames
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

    def to_s
      "#<BufferStream @buffer=#{@buffer.id}, @path=#{@path}, @size=#{@size}, @extension=#{@extension}, @sample_format=#{@sample_format}, @n_chans=#{@n_chans}>"
    end
  end
end
