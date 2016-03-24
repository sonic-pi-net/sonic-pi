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

      @mutex.synchronize do
        candidates = @cached_candidates[filts_and_sources]
        return candidates if candidates

        candidates = []
        procs = []
        filters = []
        idx = nil


        filts_and_sources.each do |arg|
          idx = consume_filt_or_source!(arg, candidates, procs, filters, idx)
        end

        if candidates.empty? && procs.empty? && filters.size == 1 && filters[0].is_a?(Symbol)

          default_samples_paths.each do |p|
            p = File.expand_path(p)
            candidates.concat(ls_samples(p)) if File.directory?(p)
          end
        end

        procs.each do |p|
          candidates = p.call(candidates)
          raise "Sample Pack Filter Proc needs to return an array or ring. Got #{candidates.class}: #{candidates.inspect}" unless candidates.is_a?(Array) || candidates.is_a?(SonicPi::Core::RingVector)
        end

        filters.each do |f|
          candidates.keep_if do |v|
            bn = File.basename(v, ".*")
            bne = File.basename(v)
            case f
            when String
              bn.downcase.include?(f.downcase) || bne == f
            when Symbol
              # match only sample extensions
              regexp = /#{f}\.(wav|aif|wave|aiff|flac)/
              bne.match(regexp)
            when Regexp
              bn.match f
            end
          end
        end



        # Grab the nth element from the list using idx
        # mod idx by candidates size if idx is specified
        candidates = [candidates[idx % candidates.size]] if(idx && !candidates.empty?)
        @cached_candidates[filts_and_sources] = candidates
        #end mutex
      end

      return candidates
    end

    def ls_samples(path, recursive=false)
      res = @cached_folder_contents[path]
      return res if res

      @folder_contents_mutex.synchronize do
        res = @cached_folder_contents[path]
        return res if res
        if recursive
          res = Dir.chdir(path) { Dir.glob("**/*.{wav,wave,aif,aiff,flac}").map {|path| File.expand_path(path) } }.sort
        else
          res = Dir.chdir(path) { Dir.glob("*.{wav,wave,aif,aiff,flac}").map {|path| File.expand_path(path) } }.sort
        end
        @cached_folder_contents[path] = res
      end
      res
    end

    private

    def default_samples_paths
      [Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_path) || @samples_path]
    end

    def consume_filt_or_source!(filt_or_source, candidates, procs, filters, idx)
      case filt_or_source
      when String
        expanded = File.expand_path(filt_or_source)
        if expanded.end_with?("**") && File.directory?(expanded[0...-2])
          candidates.concat(ls_samples(expanded[0...-2], true))
        elsif File.directory?(expanded)
          candidates.concat(ls_samples(expanded))
        elsif File.exists?(expanded)
          candidates << expanded
        else
          # Treat as a regular filter
          filters << filt_or_source
        end
      when Symbol
        filters << filt_or_source
      when Integer
        idx = filt_or_source
      when Regexp
        filters << filt_or_source
      when Proc
        if filt_or_source.arity == 0
          idx = consume_filt_or_source!(filt_or_source.call, candidates, procs, filters, idx)
        elsif filt_or_source.arity == 1
          procs << filt_or_source
        else
          raise "Sample Pack Proc needs to accept either 0 or 1 arguments. Found #{block.arity}"
        end
      when Array, SonicPi::Core::RingVector
        filt_or_source.each do |fos|
          idx = consume_filt_or_source!(fos, candidates, procs, filters, idx)
        end
      else
        raise "Unknown sample filter or source type: #{filt_or_source.inspect}"
      end

      idx
    end
  end
end
