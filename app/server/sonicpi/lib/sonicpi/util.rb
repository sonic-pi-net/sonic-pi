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
require 'fileutils'

module SonicPi
  module Util
    @@project_path = nil
    @@log_path = nil

    def os
      case RUBY_PLATFORM
      when /.*arm.*-linux.*/
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

    def default_sched_ahead_time
      if (os == :raspberry)
        1
      else
        0.2
      end
    end

    def home_dir
      File.expand_path(Dir.home + '/.sonic-pi/')
    end

    def project_path
      return @@project_path if @@project_path
      ## TODO: allow user to modify this for different projects
      path = home_dir + '/store/default/'
      ensure_dir(path)
      @@project_path = path
      path
    end

    def log_path
      return @@log_path if @@log_path
      path = home_dir + '/log/'
      ensure_dir(path)
      @@log_path = path
      path
    end

    def ensure_dir(d)
      FileUtils.mkdir_p d
    end

    def root_path
      File.absolute_path("#{File.dirname(__FILE__)}/../../../../../")
    end

    def etc_path
      File.absolute_path("#{root_path}/etc")
    end

    def doc_path
      File.absolute_path("#{etc_path}/doc")
    end

    def cheatsheets_path
      File.absolute_path("#{doc_path}/cheatsheets")
    end

    def tutorial_path
      File.absolute_path("#{doc_path}/tutorial")
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

    def app_path
      File.absolute_path("#{root_path}/app")
    end

    def html_public_path
      File.absolute_path("#{app_path}/gui/html")
    end

    def qt_gui_path
      File.absolute_path("#{app_path}/gui/qt")
    end

    def examples_path
      File.absolute_path("#{etc_path}/examples")
    end

    def server_path
      File.absolute_path("#{app_path}/server")
    end

    def native_path
      File.absolute_path("#{server_path}/native/#{os}")
    end

    def log(message)
      File.open("#{log_path}/sonicpi.log", 'a') {|f| f.write("#{Time.now.strftime("%Y-%m-%d %H:%M:%S")} #{message}\n")} if debug_mode
    end

    def debug_mode
      false
    end

    def resolve_synth_opts_hash_or_array(opts)
      case opts
      when Hash
        return opts
      when Array
        s = opts.size
        return Hash[*opts] if s.even? && s > 1
        case s
        when 1
          case opts[0]
          when Hash
            return opts[0]
          else
            raise "Invalid options. Options should either be an even list of key value pairs or a single Hash. Got #{opts.inspect}"
          end
        when 0
          return {}
        end
      else
        raise "Invalid options. Options should either be an even list of key value pairs or a single Hash. Got #{opts.inspect}"
      end
    end
  end
end
