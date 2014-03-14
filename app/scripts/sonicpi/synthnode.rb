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
require File.absolute_path("#{File.dirname(__FILE__)}/node")

module SonicPi
  class SynthNode < Node
    attr_reader :name
    def initialize(id, comms, name, arg_validation_fn = nil)
      super(id, comms, arg_validation_fn)
      @name = name
    end
  end
end
