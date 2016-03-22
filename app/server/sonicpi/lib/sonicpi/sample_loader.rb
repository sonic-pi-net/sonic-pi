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
      @cached_candidates = {}
      @cached_folder_contents = {}
      @mutex = Mutex.new
      @folder_contents_mutex = Mutex.new
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
        procs = []
        filts_and_sources.each do |arg|
          idx = consume_filt_or_source!(arg, idx, filters, dirs, candidates, procs, res)
        end

        if dirs.empty? && !filters.empty?
          dirs = default_samples_paths
        end

        dirs.each do |dir|
          path = File.expand_path(dir)
          candidates += ls_samples(path)
        end

        procs.each do |p|
          candidates = p.call(candidates)
          raise "Sample Pack Filter Proc needs to return an array or ring. Got #{res.class}: #{res.inspect}" unless candidates.is_a?(Array) || candidates.is_a?(SonicPi::Core::RingVector)
        end

        filters.each do |f|
          candidates.keep_if do |v|
            v = File.basename(v)
            case f
            when String
              v.include? f
            when Symbol
              v == f.to_s
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
      res = @cached_folder_contents[path]
      return res if res
      @folder_contents_mutex.synchronize do
        res = @cached_folder_contents[path]
        return res if res

        res = Dir.glob(path + "/*.{wav,wave,aif,aiff,flac}").sort
        @cached_folder_contents[path] = res
      end
      res
    end

    private

    def default_samples_paths
      [Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_path) || @samples_path]
    end

    def consume_filt_or_source!(filt_or_source, idx, filters, dirs, candidates, procs, res)
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

        if filt_or_source.end_with?("**") && File.directory?(File.expand_path(filt_or_source[0...-2]))
          # allow /foo/bar/baz** recursive dir matches
          dirs << filt_or_source
        elsif File.directory?(File.expand_path(filt_or_source))
          dirs << filt_or_source
        elsif File.exists?(File.expand_path(filt_or_source))
          candidates << filt_or_source
        else
          filters << filt_or_source
        end
      when Proc
        if filt_or_source.arity == 0
          consume_filt_or_source!(filt_or_source.call, idx, filters, dirs, candidates, procs, res)
        elsif filt_or_source.arity == 1
          procs << filt_or_source
        else
          raise "Sample Pack Proc needs to accept either 0 or 1 arguments. Found #{block.arity}"
        end
      when Regexp
        filters << filt_or_source
      when Array, SonicPi::Core::RingVector
        filt_or_source.each do |fos|
          idx = consume_filt_or_source!(fos, idx, filters, dirs, candidates, procs, res)
        end
      else
        raise "Unknown sample filter or source type: #{filt_or_source.inspect}"
      end

      return idx
    end
  end
end
