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
require 'multi_json'
require 'active_support/core_ext/hash/indifferent_access'

## A simple json backed settings system. Settings are persisted into a
## json file and all access and modification is synchronised with a
## Mutex.

module SonicPi
  module Config
    class Settings
      def initialize(settings_path)
        @settings_path = settings_path
        begin
          content = File.read(@settings_path)
          cur_settings =  MultiJson.load(content)
        rescue
          cur_settings = {}
        end
        @settings = cur_settings.with_indifferent_access
        @sem = Monitor.new
      end

      def get(k, default=nil)
        @sem.synchronize do
          if @settings.has_key?(k)
            @settings[k]
          else
            default
          end
        end
      end

      def get_or_set(k, default=nil)
        @sem.synchronize do
          if @settings.has_key?(k)
            @settings[k]
          else
            set(k, default)
          end
        end
      end

      def del(k)
        @sem.synchronize do
          @settings.delete(k)
          File.open(@settings_path, 'w') do |f|
            f.write(MultiJson.dump(@settings, pretty: true))
          end
        end
      end

      def set(k, v)
        @sem.synchronize do
          @settings[k] = v
          File.open(@settings_path, 'w') do |f|
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
end
