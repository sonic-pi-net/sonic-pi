#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

module SonicPi
  # Plugin hosting (VST3 and CLAP, on plugin tracks) is a build's: the GUI
  # says whether this one has it, as SONIC_PI_PLUGINS, to everything it starts
  # (app/gui/utils/plugins.h).
  module Plugins
    def self.available?
      %w[1 true yes on].include?(ENV["SONIC_PI_PLUGINS"].to_s.strip.downcase)
    end

    # A function that needs plugin hosting, called in a build without it:
    # what it is, before anything else.
    def self.require!(fn)
      raise "#{fn} needs plugin hosting, which this build of Sonic Pi doesn't have." unless available?
    end
  end
end
