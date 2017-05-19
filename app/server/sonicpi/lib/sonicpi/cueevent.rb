#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2017 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

module SonicPi
  class CueEvent

    class EmptyPathError < StandardError ; end

    include Comparable

    attr_reader :time, :beat, :thread_id, :delta, :path, :val, :meta, :split_path, :path_size
    def initialize(time, thread_id, delta, beat, path, val, meta={})
      @time = time.to_f
      @beat = beat.to_f
      @delta = delta.to_i
      @thread_id = thread_id
      if path.is_a?(Symbol)
        @path = "/cue/#{path}".strip.freeze
      else
        path = path.to_s
        path = "/#{path}" unless path.start_with?("/")
        @path = path.strip.freeze
      end
      raise EmptyPathError, "CueEvent must have a valid path. Got: #{@path}" if @path == "/"
      @val = val.__sp_make_thread_safe
      @meta = meta.__sp_make_thread_safe
      @split_path = @path[1..-1].split("/")
      @path_size = @split_path.size
    end

    def ==(other)
      other.is_a? CueEvent
      other.time == @time
      other.thread_id == @thread_id
      other.delta == @delta
      other.beat == @beat
      other.path == @path
      other.val == @val
      other.meta == @meta
    end

    def <=>(other)
      return -1 if @time < other.time
      return 1 if @time > other.time
      return -1 if @thread_id < other.thread_id
      return 1 if @thread_id > other.thread_id
      return -1 if @delta < other.delta
      return 1 if @delta > other.delta
      return 0
    end

    def path_segment(idx)
      split_path[idx]
    end

    def to_s
      "#<SonicPi::CueEvent:#{[[@time, @thread_id, @delta, @beat], @path, @val]}"
    end

    def inspect
      to_s
    end

    private

    def matcher?(s)
      s.include?('*')
    end
  end
end
