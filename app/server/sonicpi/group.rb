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
require_relative "node"

module SonicPi
  class Group < Node

    def initialize(id, comms)
      super(id, comms)
    end

    def clear
      @comms.group_clear @id
      self
    end

    def deep_clear
      @comms.group_deep_clear @id
      self
    end

    def to_s
      "#<SonicPi::Group @id=#{@id}>"
    end
  end

end
