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

    include SonicPi::Util

    def initialize(samples_paths)
      @cached_candidates = {}
      @cached_extracted_candidates = {}
      @cached_extracted_candidates_mutex = Mutex.new
      @cached_candidates_mutex = Mutex.new
      @cached_folder_contents = {}
      @mutex = Mutex.new
      @folder_contents_mutex = Mutex.new

      @samples_paths = samples_paths
      @samples_paths = [@samples_paths] unless @samples_paths.is_a?(Array) or @samples_paths.is_a?(SonicPi::Core::SPVector)
    end

    def find_candidates(filts_and_sources)
      filts_and_sources.flatten!
      filts_and_sources.compact!

      return [] if filts_and_sources.empty?

      res = @cached_candidates[filts_and_sources]
      return res if res

      orig_candidates, filters_and_procs = split_candidates_and_filts(filts_and_sources)

      candidates = extract_candidates(orig_candidates).dup
      found_proc = false

      if orig_candidates.empty?
        @samples_paths.each do |p|
          if p.end_with?("**")
            candidates.concat(ls_samples(p[0...-2], true))
          else
            candidates.concat(ls_samples(p))
          end
        end
      end
      filters_and_procs.each do |f|
        case f
        when String
          candidates.keep_if do |v|
            bn = File.basename(v, ".*")
            bn.downcase.include?(f.downcase) || (File.basename(v) == f)
          end
        when Symbol
          candidates.keep_if do |v|
            bn = File.basename(v, ".*")
            bn == f.to_s
          end
        when Regexp
          candidates.keep_if do |v|
            bn = File.basename(v, ".*")
            bn.match f
          end
        when Integer
          unless candidates.empty?
            candidates = [candidates[f % candidates.size]]
          end
        when NilClass
          # Do nothing
        when Proc
          raise "Sample Pack Proc needs to accept 1 argument only. Found #{block.arity}" unless f.arity == 1
          found_proc = true
          candidates = f.call(candidates)
          candidates = [candidates] unless is_list_like?(candidates)
        else
          raise "Unknown sample filter type: #{f.class} - got: #{f.inspect}"
        end
      end

      # don't cache contents if there's a proc as it may be stateful or
      # random and therefore the same proc might exhibit different
      # behaviour each time it is called.
      unless found_proc
        @mutex.synchronize do
          @cached_candidates[filts_and_sources] = candidates.freeze
        end
        #end mutex
      end

      return candidates
    end


    def extract_candidates(candidates)
      return [] if candidates.empty?
      cached_all_candidates = @cached_extracted_candidates[candidates]
      return cached_all_candidates if cached_all_candidates

      @cached_extracted_candidates_mutex.synchronize do
        cached_all_candidates = @cached_extracted_candidates[candidates]
        return cached_all_candidates if cached_all_candidates

        all_candidates = []

        candidates.each do |c|
          expanded = File.expand_path(c)
          if expanded.end_with?("**") && File.directory?(expanded[0...-2])
            all_candidates.concat(ls_samples(expanded[0...-2], true))
          elsif File.directory?(expanded)
            all_candidates.concat(ls_samples(expanded))
          elsif File.exist?(expanded)
            all_candidates << expanded
          else
            raise "Unknown sample candidate kind: #{expanded.inspect}. Not a file, directory or /** glob."
          end
          all_candidates_copy = all_candidates.clone
          @cached_extracted_candidates[candidates] = all_candidates_copy.freeze
        end

        return all_candidates
      end
    end


    def split_candidates_and_filts(filts_and_sources)
      candidates = []
      idx = 0
      filts_and_sources.each_with_index do |el|
        break unless el.is_a?(String)

        p = File.expand_path(el)

        if @cached_folder_contents[p] || File.exist?(p) || (p.end_with?("**") && File.directory?(p[0...-2]))
          idx += 1
          candidates << p
        else
          break
        end
      end

      return candidates, filts_and_sources[idx..-1]
    end

    def ls_samples(path, recursive=false)
      return [] unless File.directory?(path)
      res = @cached_folder_contents[path]
      return res if res

      @folder_contents_mutex.synchronize do
        res = @cached_folder_contents[path]
        return res if res
        # match wav, aiff, aif, wave, flac, ogg and oga files
        pattern = '*.{[wW][aA][vV],[wW][aA][vV][eE],[aA][iI][fF],[aA][iI][fF][fF],[fF][lL][aA][cC],[oO][gG][gGaA]}'
        if recursive
          res = Dir.chdir(path) { Dir.glob("**/#{pattern}").map { |p| File.expand_path(p) } }.sort
        else
          res = Dir.chdir(path) { Dir.glob(pattern).map { |p| File.expand_path(p) } }.sort
        end
        @cached_folder_contents[path] = res.freeze
      end
      res
    end

    def reset!
      @cached_extracted_candidates_mutex.synchronize do
        @cached_candidates_mutex.synchronize do
          @mutex.synchronize do
            @cached_extracted_candidates = {}
            @cached_candidates = {}
            @cached_folder_contents = {}
          end
        end
      end
    end
  end
end
