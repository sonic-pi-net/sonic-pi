#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, distribution,
# and distribution of modified versions of this work as long as this
# notice is included.
#++
require 'osc-ruby'

module SonicPi
  class Version

    attr_reader :major, :minor, :patch, :dev

    def initialize(major, minor, patch, dev=nil)
      @major = major.to_i
      @minor = minor.to_i
      @patch = patch.to_i
      @dev = dev
    end

    def to_s
      if @dev
        "#{@major}.#{@minor}.#{@patch}-#{@dev}"
      else
        if @patch == 0
          "#{@major}.#{@minor}"
        else
          "#{@major}.#{@minor}.#{@patch}"
        end
      end
    end

    def inspect
      to_s
    end

    def dev?
      !!@dev
    end
  end
end
