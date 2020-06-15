#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
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

    attr_reader :time, :time_r, :priority, :thread_id, :delta, :beat, :bpm, :path, :val, :meta, :split_path, :path_size

    @@path_cache = Hash.new
    @@split_path_cache = Hash.new

    def initialize(time, priority, thread_id, delta, beat, bpm, path, val, meta={})
      @time = time
      @time_r = time.to_r
      @priority = priority.to_i
      @thread_id = thread_id
      @delta = delta.to_i
      @beat = beat.to_f
      @bpm = bpm.to_f

      if path.is_a?(Symbol)
        @path = @@path_cache[path] || @@path_cache[path] = "/cue/#{path}".strip.freeze
      else
        @path = @@path_cache.fetch(path) do |k|
          path = path.to_s
          path = "/#{path}" unless path.start_with?("/")
          @@path_cache[k] = path.strip.freeze
        end
      end

      raise EmptyPathError, "CueEvent must have a valid path. Got: #{@path}" if @path == "/"
      @val = val.__sp_make_thread_safe
      @meta = meta.__sp_make_thread_safe
      @split_path = @@split_path_cache[path] || @@split_path_cache[path] = @path[1..-1].split("/")
      @path_size = @split_path.size
    end

    def ==(other)
      other.is_a?(CueEvent) &&
      (other.time_r == @time_r) &&
      (other.priority == @priority) &&
      (other.thread_id == @thread_id) &&
      (other.delta == @delta) &&
      (other.beat == @beat) &&
      (other.path == @path) &&
      (other.val == @val) &&
      (other.meta == @meta)
    end

    def <=>(other)
      return -1 if @time_r < other.time_r
      return 1 if @time_r > other.time_r
      return -1 if @priority < other.priority
      return 1 if @priority > other.priority
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
      "#<SonicPi::CueEvent:#{[[@time.to_f, @priority, @thread_id, @delta, @beat, @bpm], @path, @val]}"
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
