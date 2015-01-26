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
require 'multi_json'
require 'active_support/core_ext/hash/indifferent_access'

module SonicPi
  class Settings
    def initialize
      begin
        content = File.read(user_settings_path)
        cur_settings =  MultiJson.load(content)
      rescue
        cur_settings = {}
      end
      @settings = cur_settings.with_indifferent_access
      @sem = Mutex.new
    end

    def get(k)
      @settings[k]
    end

    def del(k)
      @sem.synchronize do
        @settings.delete(k)
        File.open(user_settings_path, 'w') do |f|
          f.write(MultiJson.dump(@settings, pretty: true))
        end
      end
    end

    def set(k, v)
      @sem.synchronize do
        @settings[k] = v
        File.open(user_settings_path, 'w') do |f|
          f.write(MultiJson.dump(@settings, pretty: true))
        end
      end
      v
    end

    def all
      @sem.synchronize do
        @settings.clone.with_indifferent_access
      end
    end
  end
end
