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
require 'cgi'

module SonicPi
  module Util
    def os
      case RUBY_PLATFORM
      when /.*armv6l-linux.*/
        :raspberry
      when /.*linux.*/
        :linux
      when /.*darwin.*/
        :osx
      when /.*mingw.*/
        :windows
      else
        raise "Unsupported platform #{RUBY_PLATFORM}"
      end
    end

    def root_path
      File.absolute_path("#{File.dirname(__FILE__)}/../../../")
    end

    def etc_path
      File.absolute_path("#{root_path}/etc")
    end

    def log_path
      File.absolute_path("#{root_path}/log")
    end

    def tmp_path
      File.absolute_path("#{root_path}/tmp")
    end

    def synthdef_path
      File.absolute_path("#{etc_path}/synthdefs")
    end

    def samples_path
      File.absolute_path("#{etc_path}/samples")
    end

    def html_public_path
      File.absolute_path("#{root_path}/app/gui/html")
    end

    def log(message)
      File.open("#{log_path}/sonicpi.log", 'a') {|f| f.write("#{Time.now.strftime("%Y-%m-%d %H:%M:%S")} #{message}\n")}
    end

    def debug_mode
      false
    end

    def resolve_synth_opts_hash_or_array(opts)
      opts = case opts
             when Hash
               opts
             when Array
               case opts.size
               when 1
                 case opts[0]
                 when Hash
                   opts[0]
                 else
                   raise "Invalid options. Options should either be an even list of key value pairs or a single Hash. Got #{opts.inspect}"
                 end
               when 0
                 {}
               else
                 raise "Number of items in options should be even - got #{opts.size}: #{opts}" if opts.size.odd?
                 Hash[*opts]
               end
             else
               raise "Invalid options. Options should either be an even list of key value pairs or a single Hash. Got #{opts.inspect}"
             end
    end
  end
end
