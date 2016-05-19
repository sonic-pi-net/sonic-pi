#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
oq# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require 'tmpdir'
require 'fileutils'
require_relative "../sonicpi/lib/sonicpi/util"
tmp_dir = Dir.tmpdir

pids_store = tmp_dir + "/sonic-pi-pids"
Dir.mkdir(pids_store) unless File.exists? pids_store

pid = ARGV[0]
pid_path = "#{pids_store}/#{pid}"

FileUtils.touch pid_path
