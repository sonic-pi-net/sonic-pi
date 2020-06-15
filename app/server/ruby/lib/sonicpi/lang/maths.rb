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

module SonicPi
  module Lang
    module Maths

      include SonicPi::Lang::Support::DocSystem

      def math_scale(val, in_min, in_max, out_min, out_max)
        (((out_max - out_min) * (val - in_min)) / (in_max - in_min)) + out_min
      end
      doc name:           :math_scale,
          introduced:     Version.new(3,0,0),
          summary:        "Linear scaling algorithm",
          args:           [[], ],
          returns:        :nil,
          opts:           nil,
          accepts_block:  false,
          doc:            "
  Scales a given input value within the specified input range to a
  corresponding value in the specified output range using the formula:

           (out_max - out_min) (val - in_min)
   f (x) = --------------------------------  + out_min
                    in_max - in_min


",



          examples:       [
        "math_scale 0.5, 0, 1, 10, 20 #=> 15"
      ]


    end
  end
end
