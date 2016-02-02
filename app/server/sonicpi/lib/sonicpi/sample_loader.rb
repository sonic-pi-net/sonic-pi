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
    def initialize
      @file_matcher = /^~?\//
      @candidates = {}
      @mutex = Mutex.new
    end

    def find_candidates(filts_and_sources)
      return [] if filts_and_sources.empty?
      candidates = @candidates[filts_and_sources]
      return candidates if candidates
      @mutex.synchronize do
        candidates = @candidates[filts_and_sources]
        return candidates if candidates
        idx = nil
        dirs = []
        filters = []
        candidates = []
        filts_and_sources.each do |arg|
          case arg
          when Symbol
            filters << /#{arg}\.(wav|aif|wave|aiff)/
          when Integer
            idx = arg
          when String
            if arg.match(@file_matcher)
              dirs << arg
            else
              filters << arg
            end
          when Regexp
            filters << arg
          when Array, RingVector
            candidates += arg
          else
            raise "Unknown sample filter or source type: #{arg.inspect}"
          end
        end

        if dirs.empty?
          dirs = [Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_path) || samples_path]
        end

        dirs.each do |dir|
          path = File.expand_path(dir)
          if File.directory?(path)
            puts "path: " + path
            puts "glob: " + Dir.glob(path + "/*.{wav,aif,wave,aiff}").inspect
            candidates += Dir.glob(path + "/*.{wav,aif,wave,aiff}")
          else
            candidates += Dir.glob(path)
          end
        end

        puts "dirs: #{dirs}"
        puts "filters: #{filters}"
        puts "candidates: #{filters}"

        filters.each do |f|
          candidates.keep_if do |v|
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

        puts "pdirs: #{dirs}"
        puts "pfilters: #{filters}"
        puts "pocandidates: #{filters}"
        candidates = [candidates[idx % candidates.size]] if idx

        @candidates[filts_and_sources] = candidates
        #end mutex
      end

      return candidates
    end

    def find_path(filts_and_sources)
      find_candidates(filts_and_sources)[0]
    end
  end
end
