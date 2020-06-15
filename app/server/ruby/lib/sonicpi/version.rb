#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

module SonicPi
  class Version
    include Comparable
    attr_reader :major, :minor, :patch, :dev

    def self.init_from_string(s)
      s = s.to_s
      m1 = /\A[vV]?([0-9]+)\Z/
      m2 = /\A[vV]?([0-9]+)\.([0-9]+)\Z/
      m3 = /\A[vV]?([0-9]+)\.([0-9]+)\.([0-9]+)\Z/
      m4 = /\A[vV]?([0-9]+)\.([0-9]+)\.([0-9]+)-(.+)\Z/

      if m = s.match(m1)
        self.new(m[1])
      elsif m = s.match(m2)
        self.new(m[1], m[2])
      elsif m = s.match(m3)
        self.new(m[1], m[2], m[3])
      elsif m = s.match(m4)
        self.new(m[1], m[2], m[3], m[4])
      else
        raise "Malformed version: #{s}. Expecting something of the form v2.1.0-RC3"
      end
    end

    def initialize(major, minor=0, patch=0, dev=nil)
      @major = major.to_i
      @minor = minor.to_i
      @patch = patch.to_i
      @dev = dev
    end

    def to_s
      if @dev
        "v#{@major}.#{@minor}.#{@patch}-#{@dev}"
      else
        if @patch == 0
          "v#{@major}.#{@minor}"
        else
          "v#{@major}.#{@minor}.#{@patch}"
        end
      end
    end

    def <=>(other)
      if ((other.is_a? Version) &&
          (@major < other.major) or
          ((@major == other.major) && (@minor < other.minor)) or
          ((@major == other.major) && (@minor == other.minor) && (@patch < other.patch)) or
          if (@dev && other.dev)
            ((@major == other.major) && (@minor == other.minor) && (@patch == other.patch) && (@dev.to_s < other.dev.to_s))
          else
            ((@major == other.major) && (@minor == other.minor) && (@patch == other.patch) && @dev)
          end)
        return -1
      elsif ((other.is_a? Version) &&
          (@major > other.major) or
          ((@major == other.major) && (@minor > other.minor)) or
          ((@major == other.major) && (@minor == other.minor) && (@patch > other.patch)) or
          if (@dev && other.dev)
            ((@major == other.major) && (@minor == other.minor) && (@patch == other.patch) && (@dev.to_s > other.dev.to_s))
          else
            ((@major == other.major) && (@minor == other.minor) && (@patch == other.patch) && other.dev)
          end)
          return 1
      elsif ((other.is_a? Version) &&
          (@major == other.major) &&
          (@minor == other.minor) &&
          (@patch == other.patch) &&
          (@dev == other.dev))
        return 0
      else
        return nil
      end
    end

    def inspect
      to_s
    end

    def dev?
      !!@dev
    end

    def to_i
      res = (@major * 100000000) + (@minor * 1000) + (@patch)
      res += 1 unless @dev
      return res
    end
  end
end
