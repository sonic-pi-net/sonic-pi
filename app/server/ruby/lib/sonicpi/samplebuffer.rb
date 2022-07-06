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

require_relative "buffer"
require_relative "util"
require_relative "sox"


module SonicPi
  class SampleBuffer < Buffer
    include Util
    def initialize(buffer, path)
      @aubio_onsets = {}
      @buffer = buffer
      @mono_buffer = nil
      @path = path
      @aubio_sem = Mutex.new
      @slices = {}
      @slices_sem = Mutex.new
      @sox_sem = Mutex.new
      @sox_info = nil
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

    def mono
      return self if num_chans == 1
      raise "implement me!"
    end

    def info
      return @sox_info if @sox_info
      @sox_sem.synchronize do
        return @sox_info if @sox_info
        @sox_info = Sox.info(@path)
      end
      return @sox_info
    end

    def onset_data
     return @aubio_onset_data if @aubio_onset_data
      @aubio_sem.synchronize do
       return @aubio_onset_data if @aubio_onset_data
        __no_kill_block do

          # These are the aubio defaults set by old gem and now
          # hard-coded into the aubio_onset binary: (this was worth
          # maintaining to preserve backwards compatibility. Might also
          # be nice to let users tweak these values in the future)

          # [:window_size]     1024
          # [:hop_size]        512
          # [:onset_threshold] 0.3
          # [:minioi_ms]       12.0 (ms)

          begin
            aubio_onsets_command = "\"#{Paths.aubio_onset_path}\" \"#{@path}\""
            onsets_str = `#{aubio_onsets_command}`
            onsets = onsets_str.split.map(&:to_f)
          rescue Exception => e
            log_exception e
            onsets = []
          end

          @aubio_onset_data = onsets.ring
        end
      end
      return @aubio_onset_data
    end

    def onsets(stretch=1)
      return @aubio_onsets[stretch] if @aubio_onsets[stretch]
      data = onset_data
      @aubio_sem.synchronize do
        return @aubio_onsets[stretch] if @aubio_onsets[stretch]
        onset_times = data.map do |el|
          [1, (el / duration)].min * stretch
        end
        @aubio_onsets[stretch] = onset_times
      end
      return @aubio_onsets[stretch]
    end

    def onset_slices
      return @aubio_slices if @aubio_slices
      ons_bounds = onsets
      @aubio_sem.synchronize do
        return @aubio_slices if @aubio_slices
        res = []
        ons_bounds << 0 if ons_bounds.empty?
        ons_bounds << 1 if ons_bounds[-1] != 1
        ons_bounds.each_cons(2).each_with_index do |(start, finish), idx|
          res << { start: start, finish: finish, index: idx }
        end
        @aubio_slices = res.ring
      end
      return @aubio_slices
    end

    def slices(num=16, start=0, finish=1)
      return @slices[[num, start, finish]] if @slices[[num, start, finish]]
      res = []
      @slices_sem.synchronize do
        return @slices[[num, start, finish]] if @slices[[num, start, finish]]

        raise "start arg must be a number, got: #{start.inspect}" unless start.is_a?(Numeric)
        raise "finish arg must be a number, got: #{finish.inspect}" unless finish.is_a?(Numeric)

        slice_size = (finish - start) / num.to_f
        prev = start
        val = start + slice_size
        num = num.to_i
        num.times do |n|
          res << {:start => prev, :finish => val, index: n}
          prev = val
          val += slice_size
        end
        res = res.ring
        @slices[[num, start, finish]] = res
      end
      return res
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
