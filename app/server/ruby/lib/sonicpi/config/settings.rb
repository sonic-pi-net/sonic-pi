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
          # get/set key on symbols, so loaded keys must be symbols too
          cur_settings = MultiJson.load(content, symbolize_keys: true)
        rescue
          cur_settings = {}
        end
        @settings = cur_settings
        @sem = Monitor.new
      end

      def get(k, default=nil)
        k = k.to_sym
        @sem.synchronize do
          v = @settings[k]
          # Treat an explicit JSON null the same as a missing key — callers
          # passing a default expect never to receive nil back.
          v.nil? ? default : v
        end
      end

      def get_or_set(k, default=nil)
        k = k.to_sym
        @sem.synchronize do
          if @settings.has_key?(k)
            @settings[k]
          else
            set(k, default)
          end
        end
      end

      def del(k)
        k = k.to_sym
        @sem.synchronize do
          @settings.delete(k)
          File.open(@settings_path, 'w') do |f|
            f.write(MultiJson.dump(@settings, pretty: true))
          end
        end
      end

      def set(k, v)
        k = k.to_sym
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
          @settings.clone
        end
      end
    end
  end
end
