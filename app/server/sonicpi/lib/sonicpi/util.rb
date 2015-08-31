#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++
require 'cgi'
require 'fileutils'
require 'securerandom'

module SonicPi
  module Util
    @@project_path = nil
    @@log_path = nil
    @@current_uuid = nil
    @@raspberry_pi_1 = RUBY_PLATFORM.match(/.*arm.*-linux.*/) && File.exists?('/proc/cpuinfo') && !(`cat /proc/cpuinfo | grep ARMv6`.empty?)

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

    def raspberry_pi_1?
      os == :raspberry && @@raspberry_pi_1
    end

    def raspberry_pi_2?
      os == :raspberry && !@@raspberry_pi_1
    end


    def num_audio_busses_for_current_os
      if os == :raspberry
        64
      else
        1024
      end

    end

    def default_sched_ahead_time
      if (os == :raspberry)
        1
      else
        0.2
      end
    end

    def host_platform_desc
      case os
      when :raspberry
        if raspberry_pi_1?
          "Raspberry Pi"
        else
          "Raspberry Pi 2"
        end
      when :linux
        "Linux"
      when :osx
        "Mac"
      when :windows
        "Win"
      end
    end

    def default_control_delta
      if raspberry_pi_1?
        0.02
      elsif raspberry_pi_2?
        0.013
      else
        0.005
      end
    end

    def home_dir
      File.expand_path('~/.sonic-pi/')
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

    def global_uuid
      return @@current_uuid if @@current_uuid
      path = home_dir + '/.uuid'
      ensure_dir(home_dir)

      if (File.exists? path)
        old_id = File.readlines(path).first.strip
        if  (not old_id.empty?) &&
            (old_id.size == 36)
          @@current_uuid = old_id
          return old_id
        end
      end

      # invalid or no uuid - create and store a new one
      new_uuid = SecureRandom.uuid
      File.open(path, 'w') {|f| f.write(new_uuid)}
      @@current_uuid = new_uuid
      new_uuid
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

    def snippets_path
      File.absolute_path("#{etc_path}/snippets")
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

    def buffers_path
      File.absolute_path("#{etc_path}/buffers")
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

    def user_settings_path
      File.absolute_path("#{home_dir}/settings.json")
    end

    def log_raw(s)
        # TODO: consider moving this into a worker thread to reduce file
        # io overhead:
      File.open("#{log_path}/debug.log", 'a') {|f| f.write("[#{Time.now.strftime("%Y-%m-%d %H:%M:%S")}] #{s}")}
    end

    def log_exception(e, context="")
      if debug_mode
        res = "Exception => #{context} #{e.message}"
        e.backtrace.each do |b|
          res << "                                        "
          res << b
          res << "\n"
        end
        log_raw res
      end
    end

    def log_info(s)
      log "--------------->  " + s
    end

    def log(message)
      if debug_mode
        res = ""
        first = true
        while !(message.empty?)
          if first
            res << message.slice!(0..151)
            res << "\n"
            first = false
          else
            res << "                                        "
            res << message.slice!(0..133)
            res << "\n"

          end
        end
        log_raw res
      end
    end

    def debug_mode
      false
    end

    def osc_debug_mode
      false
    end

    def incoming_osc_debug_mode
      false
    end

    def resolve_synth_opts_hash_or_array(opts)
      case opts
      when Hash
        return opts
      when Array
        merge_synth_arg_maps_array(opts)
      when NilClass
        return {}
      else
        raise "Invalid options. Options should either be an even list of key value pairs, a single Hash or nil. Got #{opts.inspect}"
      end
    end

    def merge_synth_arg_maps_array(opts_a)
      res = {}
      idx = 0
      size = opts_a.size

      while (idx < size) && (m = opts_a[idx]).is_a?(Hash)
        res = res.merge(m)
        idx += 1
      end

      return res if idx == size
      left = (opts_a[idx..-1])
      raise "There must be an even number of trailing synth args" unless left.size.even?
      res.merge(Hash[*left])
    end

    def arg_h_pp(arg_h)
      s = "{"
      arg_h.each do |k, v|
        rounded = v.is_a?(Float) ? v.round(4) : v
        s << "#{k}: #{rounded}, "
      end
      s.chomp(", ") << "}"
    end


  end
end
