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
      res = []
      @mutex.synchronize do

        candidates = @cached_candidates[filts_and_sources]
        return candidates if candidates

        idx = nil
        dirs = []
        filters = []
        candidates = []
        filts_and_sources.each do |arg|
          idx = consume_filt_or_source!(arg, idx, filters, dirs, candidates, res)
        end

        if dirs.empty? && !filters.empty?
          dirs = default_samples_paths
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

        candidates = [candidates[idx % candidates.size]] if(idx && !candidates.empty?)

        res = (res + candidates).uniq
        @cached_candidates[filts_and_sources] = res
        #end mutex
      end

      return res
    end

    def ls_samples(path)
      Dir.glob(path + "/*.{wav,wave,aif,aiff,flac}")
    end

    private

    def default_samples_paths
      [Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_path) || @samples_path]
    end

    def consume_filt_or_source!(filt_or_source, idx, filters, dirs, candidates, res)
      case filt_or_source
      when Symbol
        regexp = /#{filt_or_source}\.(wav|aif|wave|aiff|flac)/
        (dirs + default_samples_paths).each do |d|
          s = ls_samples(d).find {|el| el.match(regexp)}
          if s
            res << s
            break
          end
        end
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
        filt_or_source.each do |fos|
          idx = consume_filt_or_source!(fos, idx, filters, dirs, candidates, res)
        end
      else
        raise "Unknown sample filter or source type: #{filt_or_source.inspect}"
      end

      return idx
    end
  end
end
