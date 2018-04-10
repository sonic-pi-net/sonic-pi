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
  class SnippetLoader

    include SonicPi::Util

    attr_reader :snippets

    def initialize(default_path=snippets_path, user_path=user_snippets_path, quiet)
      path            = File.expand_path(path)
      user_path       = File.expand_path(user_snippets_path)
      @snippets_paths = [path, user_path]
      @quiet          = quiet
      @snippets       = {}

      load_snippets(@snippets_paths)
    end

    def load_snippets(paths)
      paths.each do |path|
        Dir["#{path}/**/*.sps"].each do |p|

          lines = File.readlines(p)
          key = nil
          completion = ""
          point_line  = 0
          point_index = 0

          while (l = lines.shift) && !(l.start_with? "# --")
            res = l.match(/\# ?([_a-z]+):(.*)/)
            if res
              k = res[1].strip
              v = res[2].strip
              if !v.empty?
                case k
                when "key"
                  key = v
                when "point_line"
                  point_line = v.to_i
                when "point_index"
                  point_index = v.to_i
                end
              end
            end
          end

          if key
            # TODO: Need to workout how to call info from here?
            # __info "Loading snippet #{key} in #{p}" unless quiet
            completion = lines.join

            __add_completion(key, completion, point_line, point_index)
          end
        end
      end
    end

    def __add_completion(k, text, point_line_offset=0, point=0)
      @snippets[k] = [text, point_line_offset, point]
    end
  end
end
