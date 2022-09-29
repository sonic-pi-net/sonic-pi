#!/usr/bin/env ruby
#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/sonic-pi-net/sonic-pi
# License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2021 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++


module SonicPi
  module Paths
    def self.user_dir
      return File.expand_path(ENV["SONIC_PI_HOME"]) if ENV["SONIC_PI_HOME"]

      # Figure out the user's home directory
      case os
      when :windows
        return File.expand_path(ENV["USERPROFILE"]) if ENV["USERPROFILE"]
        # On Windows, Ruby lets HOME take precedence if it exists, which
        # is not what Sonic Pi should do to behave like a native Windows
        # app.  To get the same path as QDir::homePath() used by the
        # GUI, we must use HOMEDRIVE and HOMEPATH instead, if they are
        # set.
        home_drive = ENV["HOMEDRIVE"]
        home_path = ENV["HOMEPATH"]
        return File.absolute_path("#{home_drive}/#{home_path}") if home_drive and home_path
        return File.expand_path(ENV["HOME"]) if ENV["HOME"]
        return File.expand_path(Dir.home)
      else
        return File.expand_path(ENV["HOME"]) if ENV["HOME"]
        return File.expand_path(Dir.home)
      end
    end

    def self.home_dir_path
      File.absolute_path("#{user_dir}/.sonic-pi/")
    end

    def self.project_path
      File.expand_path("#{home_dir_path}/store/default/")
    end

    def self.root_path
      File.absolute_path("#{File.dirname(__FILE__)}/../../../")
    end

    def self.etc_path
      File.absolute_path("#{root_path}/etc")
    end

    def self.snippets_path
      File.absolute_path("#{etc_path}/snippets")
    end

    def self.doc_path
      File.absolute_path("#{etc_path}/doc")
    end

    def self.cheatsheets_path
      File.absolute_path("#{doc_path}/cheatsheets")
    end

    def self.tutorial_path
      File.absolute_path("#{doc_path}/tutorial")
    end

    def self.tmp_path
      File.absolute_path("#{root_path}/tmp")
    end

    def self.synthdef_path
      File.absolute_path("#{etc_path}/synthdefs/compiled")
    end

    def self.samples_path
      File.absolute_path("#{etc_path}/samples")
    end

    def self.cached_samples_path
      File.absolute_path("#{project_path}/cached_samples")
    end

    def self.buffers_path
      File.absolute_path("#{etc_path}/buffers")
    end

    def self.app_path
      File.absolute_path("#{root_path}/app")
    end

    def self.html_public_path
      File.absolute_path("#{app_path}/gui/html/public")
    end

    def self.qt_gui_path
      File.absolute_path("#{app_path}/gui/qt")
    end

    def self.examples_path
      File.absolute_path("#{etc_path}/examples")
    end

    def self.server_path
      File.absolute_path("#{app_path}/server")
    end

    def self.config_path
      File.absolute_path("#{home_dir_path}/config")
    end

    def self.init_path
      File.absolute_path("#{config_path}/init.rb")
    end

    def self.original_init_path
      File.absolute_path("#{home_dir_path}/init.rb")
    end

    def self.log_path
      File.absolute_path("#{home_dir_path}/log")
    end

    def self.log_history_path
      File.absolute_path("#{log_path}/history")
    end

    def self.system_store_path
      File.absolute_path("#{home_dir_path}/store/system")
    end

    def self.server_bin_path
      File.absolute_path("#{server_path}/ruby/bin")
    end

    def self.native_path
      File.absolute_path("#{server_path}/native/")
    end

    def self.aubio_onset_path
      case os
      when :windows
        File.absolute_path("#{native_path}/aubio_onset.exe")
      else
        File.absolute_path("#{native_path}/aubio_onset")
      end
    end

    def self.sox_path
      case os
      when :windows
        File.absolute_path("#{native_path}/sox/sox.exe")
      else
        File.absolute_path("#{native_path}/sox/sox")
      end
    end

    def self.scsynth_log_path
      File.absolute_path("#{log_path}/scsynth.log")
    end

    def self.tau_log_path
      File.absolute_path("#{log_path}/tau.log")
    end

    def self.tau_boot_log_path
      File.absolute_path("#{log_path}/tau_boot.log")
    end

    def self.jackd_log_path
      File.absolute_path("#{log_path}/jackd.log")
    end

    def self.spider_log_path
      File.absolute_path("#{log_path}/spider.log")
    end

    def self.daemon_log_path
      File.absolute_path("#{log_path}/daemon.log")
    end

    def self.ruby_path
      case os
      when :windows
        has_embedded_ruby = true
        embedded_ruby_path = File.absolute_path("#{native_path}/ruby/bin/ruby.exe")
      when :macos
        has_embedded_ruby = true
        embedded_ruby_path = File.absolute_path("#{native_path}/ruby/bin/ruby")
      else
        has_embedded_ruby = false
      end

      require 'rbconfig'
      current_ruby = File.join(RbConfig::CONFIG['bindir'],
                               RbConfig::CONFIG['RUBY_INSTALL_NAME'] + RbConfig::CONFIG['EXEEXT'])
      if has_embedded_ruby && File.exist?(embedded_ruby_path)
        return embedded_ruby_path
      elsif File.exist?(current_ruby)
        return current_ruby
      else
        return "ruby"
      end
    end


    def self.mix_release_boot_path
      case os
      when :windows
        File.absolute_path("#{server_path}/beam/tau/boot-win.bat")
      when :macos
        File.absolute_path("#{server_path}/beam/tau/boot-mac.sh")
      else
        File.absolute_path("#{server_path}/beam/tau/boot-lin.sh")
      end
    end

    def self.tau_base_path
      File.absolute_path("#{server_path}/beam/tau")
    end

    def self.tau_release_path
      File.absolute_path("#{tau_base_path}/_build/prod/rel/tau/releases/0.1.0")
    end

    def self.tau_release_root
      File.absolute_path("#{tau_base_path}/_build/prod/rel/tau")
    end

    def self.tau_release_erl_bin_path
      case os
      when :windows
        base = File.absolute_path("#{tau_base_path}/_build/prod/rel/tau")
        erts_dir = Dir["#{base}/erts-*"][0]
        path = File.absolute_path("#{erts_dir}/bin/erl.exe")

        raise "Unable to find erl.exe. Did the Elixir build release work correctly? I looked here: #{path.inspect}" unless File.exist?(path)
        path
      when :macos

      else

      end
    end

    def self.tau_release_sys_config_path
      File.absolute_path("#{tau_release_path}/sys")
    end

    def self.tau_release_sys_path
      File.absolute_path("#{tau_release_path}/sys")
    end

    def self.tau_release_start_path
      File.absolute_path("#{tau_release_path}/start")
    end

    def self.tau_release_vm_args_path
      File.absolute_path("#{tau_release_path}/vm.args")
    end

    def self.tau_release_lib_path
      File.absolute_path("#{tau_base_path}/_build/prod/rel/tau/lib")
    end

    def self.tau_app_path
      File.absolute_path("#{tau_base_path}/ebin")
    end

    def self.user_audio_settings_path
      File.absolute_path("#{config_path}/audio-settings.toml")
    end

    def self.user_tau_settings_path
      File.absolute_path("#{config_path}/tau-settings.toml")
    end

    def self.system_cache_store_path
      File.absolute_path("#{system_store_path}/cache.json")
    end

    def self.user_config_examples_path
      File.absolute_path("#{app_path}/config/user-examples")
    end

    def self.spider_server_path
      File.absolute_path("#{server_bin_path}/spider-server.rb")
    end

    def self.scsynth_path
      case os
      when :linux
        "scsynth"
      when :macos
        path = "#{native_path}/scsynth"
        raise "Unable to find SuperCollider. Is it installed? I looked here: #{path.inspect}" unless File.exist?(path)
        path
      when :windows
        path = "#{native_path}/scsynth.exe"
        raise "Unable to find SuperCollider. Is it installed? I looked here: #{path.inspect}" unless File.exist?(path)
        path
      end
    end

    def self.scsynth_windows_plugin_path
      File.absolute_path("#{native_path}/plugins")
    end

    def self.scsynth_raspberry_plugin_path
      "/usr/lib/SuperCollider/plugins"
    end

    def self.os
      case RUBY_PLATFORM
      when /.*linux.*/
        :linux
      when /.*darwin.*/
        :macos
      when /.*mingw.*/
        :windows
      else
        raise "Unsupported platform #{RUBY_PLATFORM}"
      end
    end
  end
end
