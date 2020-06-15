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

require_relative "util"
require 'open3'

module SonicPi
  module Sox
    include Util

    def self.info(path)
      # Some sox commands print out to std err?!
      info_out, info_err = Open3.capture3("'#{sox_path}' --info '#{path}'")
      stat_out, stat_err = Open3.capture3("'#{sox_path}' '#{path}' -n stat")
      res = {}
      (info_out.lines + info_err.lines + stat_out.lines + stat_err.lines).each do |l|
        m = l.match(/\A(.+?)\s*:\s+(.*)\Z/)

        if m
          k = m[1]
          k = k.downcase
          k = k.gsub(/[^a-z0-9_]+/, " ")
          k = k.gsub(/\s+/, " ")
          k = k.strip
          k = k.gsub(/\s+/, "_")
          k = k.to_sym

          puts k
          begin
            res[k] = Float(m[2])
          rescue
            res[k] = m[2]
          end
        end
      end

      return res.to_sp_map
    end

    def self.mono_mix(path, destination=nil)

    end
  end
end
