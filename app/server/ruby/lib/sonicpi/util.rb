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
require 'cgi'
require 'fileutils'
require 'securerandom'

module SonicPi
  module Util
    # Check which OS we're on
    case RUBY_PLATFORM
    when /.*arm.*-linux.*/
      @@os = :raspberry
    when /aarch64.*linux.*/
       @@os = :raspberry
    when /.*linux.*/
      @@os = :linux
    when /.*darwin.*/
      @@os = :osx
    when /.*mingw.*/
      @@os = :windows
    else
      raise "Unsupported platform #{RUBY_PLATFORM}"
    end

    # Figure out the user's home directory
    case @@os
    when :windows
      # On Windows, Ruby lets HOME take precedence if it exists, which is
      # not what Sonic Pi should do to behave like a native Windows app.
      # To get the same path as QDir::homePath() used by the GUI, we must
      # use HOMEDRIVE and HOMEPATH instead, if they are set.
      home_drive = ENV["HOMEDRIVE"]
      home_path = ENV["HOMEPATH"]
      if home_drive and home_path
        @@user_dir = home_drive + home_path
      else
        @@user_dir = Dir.home
      end
    else
        @@user_dir = Dir.home
    end

    @@safe_mode = false
    @@project_path = nil
    @@log_path = nil
    @@current_uuid = nil
    @@home_dir = nil
    @@util_lock = Mutex.new
    @@raspberry_pi_1 = RUBY_PLATFORM.match(/.*arm.*-linux.*/) && File.exist?('/proc/cpuinfo') && !(`cat /proc/cpuinfo | grep BCM2708`).empty?
    @@raspberry_pi_2 = RUBY_PLATFORM.match(/.*arm.*-linux.*/) && ['a01040','a01041','a22042'].include?(`awk '/^Revision/ { print $3}' /proc/cpuinfo`.delete!("\n"))
    @@raspberry_pi_3 = RUBY_PLATFORM.match(/.*arm.*-linux.*/) && ['a02082','a22082','a32082'].include?(`awk '/^Revision/ { print $3}' /proc/cpuinfo`.delete!("\n"))
    @@raspberry_pi_3bplus = RUBY_PLATFORM.match(/.*arm.*-linux.*/) && ['a020d3'].include?(`awk '/^Revision/ { print $3}' /proc/cpuinfo`.delete!("\n"))
    @@raspberry_pi_4_1gb =  RUBY_PLATFORM.match(/.*arm.*-linux.*/) && ['a03111'].include?(`awk '/^Revision/ { print $3}' /proc/cpuinfo`.delete!("\n"))
    @@raspberry_pi_4_2gb =  RUBY_PLATFORM.match(/.*arm.*-linux.*/) && ['b03111','b03112'].include?(`awk '/^Revision/ { print $3}' /proc/cpuinfo`.delete!("\n"))
    @@raspberry_pi_4_4gb =  RUBY_PLATFORM.match(/.*arm.*-linux.*/) && ['c03111','c03112'].include?(`awk '/^Revision/ { print $3}' /proc/cpuinfo`.delete!("\n"))
   @@raspberry_pi_4_8gb =  RUBY_PLATFORM.match(/.*arm.*-linux.*/) && ['d03114'].include?(`awk '/^Revision/ { print $3}' /proc/cpuinfo`.delete!("\n"))
   @@raspberry_pi_4_8gb_64 =  RUBY_PLATFORM.match(/aarch64.*linux.*/) && ['d03114'].include?(`awk '/^Revision/ { print $3}' /proc/cpuinfo`.delete!("\n"))
    @@home_dir = File.expand_path((ENV['SONIC_PI_HOME'] || @@user_dir) + '/.sonic-pi/')
    @@project_path = @@home_dir + '/store/default/'
    @@log_path = @@home_dir + '/log/'

    @@cached_samples_path = File.absolute_path("#{@@project_path}/cached_samples")

    [@@home_dir, @@project_path, @@log_path, @@cached_samples_path].each do |dir|

      begin
        FileUtils.mkdir_p(dir) unless File.exist?(dir)
      rescue
        @@safe_mode = true
        STDERR.puts "Unable to create #{dir} due to permissions errors"
      end
    end

    begin
      @@log_file = File.open("#{@@log_path}/debug.log", 'a')
    rescue
      @@safe_mode = true
      STDERR.puts "Unable to open log file #{@@log_path}/debug.log"
      @@log_file = nil
    end

    begin
      @@process_log_file = File.open("#{@@log_path}/processes.log", 'a')
    rescue
      @@safe_mode = true
      STDERR.puts "Unable to open process log file #{@@log_path}/processes.log"
      @@process_log_file = nil
    end

    at_exit do
      @@log_file.close if @@log_file
      @@process_log_file.close if @@process_log_file
    end

    def os
      @@os
    end

    def raspberry_pi?
      os == :raspberry
    end

    def raspberry_pi_1?
      os == :raspberry && @@raspberry_pi_1
    end

    def raspberry_pi_2?
      os == :raspberry && @@raspberry_pi_2
    end

    def raspberry_pi_3?
      os == :raspberry && @@raspberry_pi_3
    end

    def raspberry_pi_3bplus?
      os == :raspberry && @@raspberry_pi_3bplus
    end

    def raspberry_pi_4_1gb?
      os == :raspberry && @@raspberry_pi_4_1gb
    end

    def raspberry_pi_4_2gb?
      os == :raspberry && @@raspberry_pi_4_2gb
    end

    def raspberry_pi_4_4gb?
      os == :raspberry && @@raspberry_pi_4_4gb
    end

    def raspberry_pi_4_8gb?
      os == :raspberry && @@raspberry_pi_4_8gb
    end

    def raspberry_pi_4_8gb_64?
      os == :raspberry && @@raspberry_pi_4_8gb_64
    end

    def unify_tilde_dir(path)
      if os == :windows
        path
      else
        path.gsub(/\A#{@@user_dir}/, "~")
      end
    end

    def num_buffers_for_current_os
      4096
    end

    def num_audio_busses_for_current_os
      if os == :raspberry && @@raspberry_pi_1
        64
      else
        1024
      end

    end

    def default_sched_ahead_time
      if raspberry_pi_1?
        1
      else
        0.5
      end
    end

    def host_platform_desc
      case os
      when :raspberry
        if raspberry_pi_1?
          "Raspberry Pi 1"
        elsif raspberry_pi_2?
          "Raspberry Pi 2B"
        elsif raspberry_pi_3?
          "Raspberry Pi 3B"
        elsif raspberry_pi_3bplus?
          "Raspberry Pi 3B+"
        elsif raspberry_pi_4_1gb?
          "Raspberry Pi 4B:1Gb"
        elsif raspberry_pi_4_2gb?
          "Raspberry Pi 4B:2Gb"
        elsif raspberry_pi_4_4gb?
          "Raspberry Pi 4B:4Gb"
        elsif raspberry_pi_4_8gb?
          "Raspberry Pi 4B:8Gb"
        elsif raspberry_pi_4_8gb_64?
          "Raspberry Pi 4B:8Gb 64bit OS"
        else
          "Raspberry Pi"
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
      if raspberry_pi?
        if raspberry_pi_1?
          0.02
        else
          0.013
        end
      else
        0.005
      end
    end

    def home_dir
      @@home_dir
    end

    def init_path
      home_dir + '/init.rb'
    end

    def project_path
      @@project_path
    end

    def cached_samples_path
      @@cached_samples_path
    end


    def log_path
      @@log_path
    end

    def global_uuid
      return @@current_uuid if @@current_uuid
      @@util_lock.synchronize do
        return @@current_uuid if @@current_uuid
        path = home_dir + '/.uuid'

        if (File.exist? path)
          old_id = File.readlines(path).first.strip
          if  (not old_id.empty?) &&
              (old_id.size == 36)
            @@current_uuid = old_id
            return old_id
          end
        end

        # invalid or no uuid - create and store a new one
        new_uuid = SecureRandom.uuid
        begin
          File.open(path, 'w') {|f| f.write(new_uuid)}
        rescue
          @@safe_mode = true
          log "Unable to write uuid file to #{path}"
        end
        @@current_uuid = new_uuid
        new_uuid
      end
    end

    def ensure_dir(dir)
      begin
        FileUtils.mkdir_p(dir) unless File.exist?(dir)
      rescue
        @@safe_mode = true
        log "Unable to create #{dir} due to permissions errors"
      end
    end

    def __exe_fix(path)
      case os
      when :windows
        "#{path}.exe"
      else
        path
      end
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
      File.absolute_path("#{etc_path}/synthdefs/compiled")
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
      File.absolute_path("#{app_path}/gui/html/public")
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

    def server_bin_path
      File.absolute_path("#{server_path}/ruby/bin")
    end

    def native_path
      File.absolute_path("#{server_path}/native/")
    end

    def sox_path
      File.join(native_path, "sox", __exe_fix("sox"))
    end

    def osmid_o2m_path
      File.join(native_path, "osmid", __exe_fix("o2m"))
    end

    def osmid_m2o_path
      File.join(native_path, "osmid", __exe_fix("m2o"))
    end

    def scsynth_log_path
      log_path + '/scsynth.log'
    end

    def erlang_log_path
      log_path + '/erlang.log'
    end

    def osmid_m2o_log_path
      log_path + '/osmid_m2o.log'
    end

    def osmid_o2m_log_path
      log_path + '/osmid_o2m.log'
    end

    def ruby_path
      case os
      when :windows
        File.join(native_path, "ruby", "bin", "ruby.exe")
      when :osx
        File.join(native_path, "ruby", "bin", "ruby")
      when  :raspberry, :linux
        "ruby"
      end
    end

    def erlang_boot_path
      case os
      when :windows
        erlang_bin_path = "\"#{File.join(native_path, "erlang", "bin", "erl.exe")}\""
      when :osx
        erlang_bin_path = File.join(native_path, "erlang", "erl")
        "\"#{ruby_path}\" \"#{erlang_bin_path}\""
        # Uncomment this if you want to use the system Erlang:
        #"erl"
      when :raspberry, :linux
        "erl"
      end
    end

    def erlang_server_path
      File.join(server_path, "erlang", "sonic_pi_server", "ebin")
    end

    def user_settings_path
      File.absolute_path("#{home_dir}/settings.json")
    end

    def fetch_url(url, anonymous_uuid=true)
      begin

          params = {:ruby_platform => RUBY_PLATFORM,
                    :ruby_version => RUBY_VERSION,
                    :ruby_patchlevel => RUBY_PATCHLEVEL,
                    :sonic_pi_version => @version.to_s}

        params[:uuid] = global_uuid if anonymous_uuid

        uri = URI.parse(url)
        uri.query = URI.encode_www_form(params)
        Net::HTTP.get_response uri
      rescue
        nil
      end
    end

    def log_raw(s)
      if @@log_file
        @@log_file.write("[#{Time.now.strftime("%Y-%m-%d %H:%M:%S")}] #{s}")
        @@log_file.flush
      else
        Kernel.puts("[#{Time.now.strftime("%Y-%m-%d %H:%M:%S")}] #{s}")
      end
    end

    def log_exception(e, context="")
      if debug_mode
        res = String.new("Exception => #{context} #{e.message}")
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
        message = String.new(message.to_s)
        res = String.new
        res << "\n" if message.empty?
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

    def log_process_info(s)
      puts s
      if @@process_log_file
        @@process_log_file.puts s
        @@process_log_file.flush
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
      when Hash, SonicPi::Core::SPMap
        return opts
      when Array, SonicPi::Core::SPVector
        merge_synth_arg_maps_array(opts)
      when NilClass
        return {}
      else
        raise "Invalid options. Options should either be an even list of key value pairs, a single Hash or nil. Got #{opts.inspect}"
      end
    end

    def truthy?(val)

        case val
        when Numeric
          return val != 0
        when NilClass
          return false
        when TrueClass
          return true
        when FalseClass
          return false
        when Proc
          new_v = val.call
          return truthy?(new_v)
        end
    end

    def zipmap(a, b)
      res = {}
      a_size = a.size
      b_size = b.size
      iters = if a_size < b_size
                a_size
              else
                b_size
              end

      iters.times do |i|
        res[a[i]] = b[i]
      end

      res
    end

    def split_params_and_merge_opts_array(opts_a)
      return [], opts_a if opts_a.is_a? Hash

      opts_a = opts_a.to_a
      params = []
      idx = 0
      size = opts_a.size

      while (idx < size) && !(m = opts_a[idx]).is_a?(Hash)
        params << m
        idx += 1
      end

      return params, {} if idx == size

      opts = (opts_a[idx..-1]).reduce({}) do |s, el|
        s.merge(el)
      end

      return params, opts
    end



    def merge_synth_arg_maps_array(opts_a)
      return opts_a if opts_a.is_a? Hash

      # merge all initial hash elements
      # assumes rest of args are kv pairs and turns
      # them into hashes too and merges the
      opts_a = opts_a.to_a
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
      h = Hash[*left]
      res.merge(h)
    end

    def purge_nil_vals!(m)
      m.delete_if { |k, v| v.nil? }
    end

    def pp_el_or_list(l)
      if l.size == 1
        return l[0].inspect
      else
        return l.inspect
      end
    end

    def arg_h_pp(arg_h)
      s = "{"
      arg_h.each do |k, v|
        if v
          rounded = v.is_a?(Float) ? v.round(4) : v.inspect
          s += "#{k}: #{rounded}, "
        end
      end
      s.chomp(", ") << "}"
    end

    def safe_mode?
      @@safe_mode
    end

    def is_list_like?(o)
      o.is_a?(Array) || o.is_a?(SonicPi::Core::SPVector)
    end

    def register_process(pid)
      pid = spawn "'#{ruby_path}' '#{File.join(server_bin_path, 'task-register.rb')}' #{pid}"
      Process.waitpid(pid, Process::WNOHANG)
    end

    def kill_and_deregister_process(pid)
      pid = spawn "'#{ruby_path}' '#{File.join(server_bin_path, 'task-clear.rb')}' #{pid}"
      Process.wait pid
    end

    def __thread_locals(t = Thread.current)
      tls = t.thread_variable_get(:sonic_pi_thread_locals)
      tls = t.thread_variable_set(:sonic_pi_thread_locals, SonicPi::Core::ThreadLocal.new) unless tls
      return tls
    end

    def __system_thread_locals(t = Thread.current)
      tls = t.thread_variable_get(:sonic_pi_system_thread_locals)
      tls = t.thread_variable_set(:sonic_pi_system_thread_locals, SonicPi::Core::ThreadLocal.new) unless tls
      return tls
    end

    def __thread_locals_reset!(tls, t = Thread.current)
      t.thread_variable_set(:sonic_pi_thread_locals, tls)
    end

    def __system_thread_locals_reset!(tls, t = Thread.current)
      t.thread_variable_set(:sonic_pi_system_thread_locals, tls)
    end

    def __no_kill_block(t = Thread.current, &block)
      mut = __system_thread_locals(t).get(:sonic_pi_local_spider_no_kill_mutex)

      # just call block when in a non-sonic-pi-thread
      return block.call unless mut

      # if we're already in a no_kill_block, run code anyway
      return block.call if __system_thread_locals(t).get(:sonic_pi_local_spider_in_no_kill_block)

      mut.synchronize do
        __system_thread_locals(t).set_local(:sonic_pi_local_spider_in_no_kill_block, true)
        begin
          r = block.call
        rescue Exception => e
          log_exception e, "in no kill block"
        ensure
          __system_thread_locals(t).set_local(:sonic_pi_local_spider_in_no_kill_block, false)
        end
        r
      end
    end
  end
end
