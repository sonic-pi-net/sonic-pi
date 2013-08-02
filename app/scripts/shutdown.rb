#!/usr/bin/env ruby

load(File.absolute_path("#{File.dirname(__FILE__)}/util.rb"))

#Kill any existing services that belong to us
kill_scsynth
kill_jackd

spider_log "All systems down"
spider_log "-=-=-=-=-=-=-=-=-"
spider_log ""
