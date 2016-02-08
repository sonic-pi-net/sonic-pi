#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

module SonicPi
  class SampleLoader
    def initialize(samples_path)
      # Matches following path prefixes:
      # ~/
      # /
      # C:\
      @file_matcher =  /(~\/|\/|[A-Za-z]:\\)/
      @cached_candidates = {}
      @mutex = Mutex.new
      @samples_path = samples_path
    end

    def find_candidates(filts_and_sources)
      return [] if filts_and_sources.empty?
      candidates = @cached_candidates[filts_and_sources]
      return candidates if candidates
      @mutex.synchronize do
        candidates = @cached_candidates[filts_and_sources]
        return candidates if candidates
        idx = nil
        dirs = []
        filters = []
        candidates = []
        filts_and_sources.each do |arg|
          idx = consume_filt_or_source!(arg, idx, filters, dirs, candidates)
        end

        if dirs.empty? && !filters.empty?
          dirs = [Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_path) || @samples_path]
        end

        dirs.each do |dir|
          path = File.expand_path(dir)
          if File.directory?(path)
            candidates += ls_samples(path)
          else
            candidates += Dir.glob(path)
          end
        end

        filters.each do |f|
          candidates.keep_if do |v|
            v = File.basename(v)
            case f
            when String
              v.include? f
            when Symbol
              v.include? f.to_s
            when Regexp
              v.match f
            end
          end
        end

        candidates = [candidates[idx % candidates.size]] if idx

        @cached_candidates[filts_and_sources] = candidates
        #end mutex
      end

      return candidates
    end

    def ls_samples(path)
      Dir.glob(path + "/*.{wav,aif,wave,aiff}")
    end

    private
    def consume_filt_or_source!(filt_or_source, idx, filters, dirs, candidates)
      case filt_or_source
      when Symbol
        filters << /#{filt_or_source}\.(wav|aif|wave|aiff)/
      when Integer
        idx = filt_or_source
      when String
        if filt_or_source.match(@file_matcher)
          if File.directory?(File.expand_path(filt_or_source))
            dirs << filt_or_source
          else
            candidates << filt_or_source
          end
        else
          filters << filt_or_source
        end
      when Proc
        consume_filt_or_source!(filt_or_source.call, filters, dirs, candidates)
      when Regexp
        filters << filt_or_source
      when Array, SonicPi::Core::RingVector
        filt_or_source.each do |c|
          if File.directory?(c)
            dirs << c
          else
            candidates << c
          end
        end
      else
        raise "Unknown sample filter or source type: #{filt_or_source.inspect}"
      end

      return idx
    end
  end
end
