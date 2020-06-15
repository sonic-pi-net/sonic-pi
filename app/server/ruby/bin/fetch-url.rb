#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2019 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "../core.rb"
require_relative "../lib/sonicpi/util"
require_relative "../lib/sonicpi/config/settings"
require 'net/http'

include SonicPi::Util

url = ARGV[0]
check_window_size = ARGV[1].to_i

settings = SonicPi::Config::Settings.new(user_settings_path)
body = settings.get("cached_url_#{url}")
last_checked = settings.get("cached_url_last_checked_#{url}", Time.now).to_i

if ((Time.now.to_i - last_checked) > check_window_size) || (body == nil)
  puts 'forcing recheck'
  # need to do a fresh check
  response = fetch_url(url)
  if response
    body = response.body
  else
    body = ""
  end
  settings.set("cached_url_last_checked_#{url}", Time.now.to_i)
  settings.set("cached_url_#{url}", body)
end

puts body if body
