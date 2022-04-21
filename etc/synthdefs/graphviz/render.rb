#!/usr/bin/ruby -wKU

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

path = File.expand_path(File.dirname(__FILE__))

Dir["#{path}/*.dot"].each do |f|
#  `dot #{f} -Tpng -o #{f[0..-5]}.png`
  `dot #{f} -Tpdf -o #{f[0..-5]}.pdf`
end
