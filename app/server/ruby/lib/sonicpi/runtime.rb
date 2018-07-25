#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++
require_relative "util"
require_relative "studio"
require_relative "incomingevents"
require_relative "counter"
require_relative "promise"
require_relative "jobs"
require_relative "synths/synthinfo"
require_relative "lang/sound"
require_relative "gitsave"
require_relative "lifecyclehooks"
require_relative "version"
require_relative "sthread"
require_relative "version"
require_relative "config/settings"
require_relative "preparser"
require_relative "spsym"
require_relative "event_history"
require_relative "thread_id"

#require_relative "oscevent"
#require_relative "stream"

require 'net/http'
require 'uri'
require 'thread'
require 'fileutils'
require 'set'
require 'ruby-beautify'
require 'securerandom'
require 'active_support/core_ext/integer/inflections'

module SonicPi
  class Stop < StandardError ; end

  module RuntimeMethods

    ## Not officially part of the API
    ## Probably should be moved somewhere else
    @@stop_job_mutex = Mutex.new

    def load_snippets(path=snippets_path, quiet=false)
      path = File.expand_path(path)
      Dir["#{path}/**/*.sps"].each do |p|

        lines = File.readlines(p)
        key = nil
        completion = ""
        point_line  = 0
        point_index = 0

        while (l = lines.shift) && !(l.start_with? "# --")
          res = l.match(/\# ?([_a-z]+):(.*)/)
          if res
            k = res[1].strip
            v = res[2].strip
            if !v.empty?
              case k
              when "key"
                key = v
              when "point_line"
                point_line = v.to_i
              when "point_index"
                point_index = v.to_i
              end
            end
          end
        end

        if key
          __info "Loading snippet #{key} in #{p}" unless quiet
          completion = lines.join

          __add_completion(key, completion, point_line, point_index)
        end
      end
    end

    def __stop_cue_server!(silent=false)
      @osc_cue_server_mutex.synchronize do
        if @osc_server
          __info "Stopping OSC server...." unless silent
          @osc_server.stop
          @osc_server = SonicPi::OSC::UDPServer.new(@osc_cues_port, open: false)
        end
      end
    end

    def __restart_cue_server!(open=false, silent=false)
      @osc_cue_server_mutex.synchronize do
        @osc_server.stop if @osc_server
        __info "Restarting OSC server...." unless silent
        @osc_server = SonicPi::OSC::UDPServer.new(@osc_cues_port, open: open,) do |address, args, info|
          address = "/#{address}" unless address.start_with?("/")
          address = "/osc:#{info[2]}:#{info[1]}#{address}"
          p = 0
          d = 0
          b = 0
          m = 60
          @register_cue_event_lambda.call(Time.now, p, @system_init_thread_id, d, b, m, address, args, 0)
        end

        unless silent
          if open
            __info "OSC server started. Listening for local and remote messages on port #{@osc_cues_port}."
          else
            __info "OSC server started. Listening for local messages on port #{@osc_cues_port}"
          end
        end

      end
    end

    def __gui_heartbeat(id)
      t = Time.now.freeze
      @gui_heartbeats[id] = t
      @gui_last_heartbeat = t
    end

    def __extract_git_hash
      head_path = root_path + "/.git/HEAD"
      if File.exist? head_path
        ref = File.readlines(head_path).first
        ref_path = root_path + "/.git/" + ref[5..-1]
        ref_path = ref_path.strip
        if File.exist? ref_path
          return File.readlines(ref_path).first
        end
      end
    end

    def __update_gui_version_info_now
      __info "Checking for new version of Sonic Pi"
      v = __check_for_server_version_now
      if @version < v
        __print_version_outdated_info(v)
      else
        __info "Your version of Sonic Pi is the latest: #{@version}"
      end

      __msg_queue.push({:type => :version, :version => @version.to_s, :version_num =>  @version.to_i, :latest_version => v.to_s, :latest_version_num => v.to_i, :last_checked => __last_update_check})

    end

    def __current_version
      @version
    end

    def __last_update_check
      Time.at(@settings.get(:last_update_check_time).to_i)
    end

    def __check_for_server_version_now

      begin
        params = {:uuid => global_uuid,
                  :ruby_platform => RUBY_PLATFORM,
                  :ruby_version => RUBY_VERSION,
                  :ruby_patchlevel => RUBY_PATCHLEVEL,
                  :sonic_pi_version => @version.to_s}
        ver_uri = URI.parse(url="http://sonic-pi.net/static/info/latest_version.txt")
        msg_uri = URI.parse(url="http://sonic-pi.net/static/info/message.txt")
        ver_uri.query = URI.encode_www_form( params )
        msg_uri.query = URI.encode_www_form( params )
        ver_response = Net::HTTP.get_response ver_uri
        msg_response = Net::HTTP.get_response msg_uri
        ver = ver_response.body
        v = Version.init_from_string(ver)
        msg = msg_response.body
        @settings.set(:last_update_check_time, Time.now.to_i)
        @settings.set(:last_seen_server_version, v.to_s)
        @settings.set(:message, msg)
        v
      rescue
        __local_cached_server_version
      end
    end

    def __server_version
      return Version.new(0) if @settings.get(:no_update_checking)

      # Only check for updates at most once every 2 weeks
      last_update = @settings.get(:last_update_check_time).to_i
      if  (last_update > 0) &&
          (Time.at(last_update) < Time.now)
        two_weeks_in_seconds = 60 * 60 * 24 * 14
        ts_2_weeks_later = Time.at(last_update + two_weeks_in_seconds)
        return __local_cached_server_version if Time.now < ts_2_weeks_later
      end

      __check_for_server_version_now
    end

    def __local_cached_server_version
      begin
        return Version.init_from_string(@settings.get(:last_seen_server_version))
      rescue
        return Version.new(0)
      end
    end

    def __print_version_outdated_info(v=@server_version)
      __info "--- IMPORTANT NOTICE ---\n\n   Your version of Sonic Pi is outdated\n   The latest is #{v}\n   Please consider updating:\n\n   http://sonic-pi.net\n\n", 1
    end

    def __info(s, style=0)
      __msg_queue.push({:type => :info, :style => style, :val => s.to_s}) unless __system_thread_locals.get :sonic_pi_spider_silent
    end

    def __multi_message(m, thread_name="")
      __msg_queue.push({:type => :multi_message, :val => m, :jobid => __current_job_id, :jobinfo => __current_job_info, :runtime => __current_local_run_time.round(4), :thread_name => thread_name}) unless __system_thread_locals.get :sonic_pi_spider_silent
    end

    def __delayed(&block)
      raise "Can only use __delayed in a job thread" unless __current_job_id
      delayed_blocks = __system_thread_locals.get :sonic_pi_local_spider_delayed_blocks
      delayed_blocks << block
    end

    def __delayed_message(s)
      __enqueue_multi_message(0, s)
    end

    def __delayed_highlight_message(s)
      __enqueue_multi_message(4, s)
    end

    def __delayed_highlight2_message(s)
      __enqueue_multi_message(5, s)
    end

    def __delayed_highlight3_message(s)
      __enqueue_multi_message(6, s)
    end

    def __delayed_user_message(s)
      s = s.inspect unless s.is_a? String
      __enqueue_multi_message(1, s)
    end

    def __delayed_serious_warning(s)
      __enqueue_multi_message(3, s)
    end

    def __delayed_warning(s)
      __enqueue_multi_message(2, s)
    end

    def __current_bpm
      60.0 / __system_thread_locals.get(:sonic_pi_spider_sleep_mul, 60)
    end

    def __current_sched_ahead_time
      #TODO: insert thread id and delta correctly
      __system_thread_locals.get(:sonic_pi_spider_sched_ahead_time) ||
        @system_state.get(__system_thread_locals.get(:sonic_pi_spider_time), 0, __current_thread_id, 0, __system_thread_locals.get(:sonic_pi_spider_beat), __current_bpm, :sched_ahead_time,).val
    end

    def __schedule_delayed_blocks_and_messages!
      delayed_messages = __system_thread_locals.get :sonic_pi_local_spider_delayed_messages
      delayed_blocks = __system_thread_locals.get(:sonic_pi_local_spider_delayed_blocks) || []
      if delayed_messages
        unless(delayed_messages.empty?)
          thread_name = __current_thread_name
          in_thread do
            last_vt = __system_thread_locals.get :sonic_pi_spider_time
            sched_ahead_sync_t = last_vt + __current_sched_ahead_time
            sleep_time = sched_ahead_sync_t - Time.now
            Kernel.sleep(sleep_time) if sleep_time > 0
            #We're now in sync with the sched_ahead time

            delayed_blocks.each do |b|
              begin
                b.call
              rescue => e
                log e.backtrace
              end
            end
            __multi_message(delayed_messages, thread_name)
          end
          __system_thread_locals.set_local :sonic_pi_local_spider_delayed_messages, []
        end
      end
    end

    def __enqueue_multi_message(m_type, m)
      raise "Can only use __enqueue_multi_message in a job thread" unless __current_job_id
      delayed_messages = __system_thread_locals.get :sonic_pi_local_spider_delayed_messages
      delayed_messages << [m_type, m]
    end

    def __extract_line_of_error(e)
      trace = e.backtrace
      return -1 unless trace

      l = trace.find {|line| line.include?("in __spider_eval")}
      unless l
        return -1
      else
        m = l.match(/.*:([0-9]+):/)
        if m
          return m[1].to_i
        else
          return -1
        end
      end
    end

    def __error(e, m=nil)
      line = __extract_line_of_error(e)
      err_msg = e.message
      info = __current_job_info
      err_msg.gsub(/for #<SonicPiSpiderUser[a-z0-9:]+>/, '')
      res = ""
      w = info[:workspace]
      if line != -1

        # TODO: Remove this hack when we have projects
        w = normalise_buffer_name(w)
        w = "buffer " + w
        # TODO: end of hack

        res = res + "[#{w}, line #{line}]"
      else
        res = res + "[#{w}]"
      end
      res += " - #{e.class}"
      res = res + "\n" + m if m
      res = res + "\n #{err_msg}"
      __msg_queue.push({type: :error, val: res, backtrace: e.backtrace, jobid: __current_job_id, jobinfo: __current_job_info, line: line})
    end

    def __current_run_time
      (__system_thread_locals.get(:sonic_pi_spider_time) - @global_start_time).round(6)
    end

    def __current_local_run_time
      (__system_thread_locals.get(:sonic_pi_spider_time) - __system_thread_locals.get(:sonic_pi_spider_start_time)).round(6)
    end

    def __current_thread_name
      __system_thread_locals.get(:sonic_pi_local_spider_users_thread_name) || ""
    end

    def __current_job_id
      __system_thread_locals.get :sonic_pi_spider_job_id
    end

    def __current_job_info
      __system_thread_locals.get(:sonic_pi_spider_job_info) || {}
    end

    def __sync(id, res)
      @cue_events.event("/sync", {:id => id, :result => res})
    end

    def __cue_events
      @cue_events
    end

    def __stop_job(j)
      __info "Stopping run #{j}"
      # Only allow a job to be stopped once
      @@stop_job_mutex.synchronize do
        if @user_jobs.running?(j)
          job_subthreads_kill(j)
          @life_hooks.killed(j)
          @user_jobs.kill_job j
          __msg_queue.push({type: :job, jobid: j, action: :killed})
        end
      end
    end

    def __stop_jobs
      __info "Stopping all runs..."
      @user_jobs.each_id do |id|
        __stop_job id
      end
      # Flush OSC messages on Erlang scheduler
      __osc_flush!

      # Force a GC collection now everything has stopped
      GC.start
    end

    def __osc_flush!
      @osc_server.send("localhost", @osc_router_port, "/flush", "default")
    end

    def __stop_other_jobs
      __info "Stopping all runs other than #{__current_job_id}..."
      @user_jobs.each_id do |id|
        __stop_job id unless id == __current_job_id
      end
    end

    def __join_subthreads(t)
      subthreads = __system_thread_locals(t).get :sonic_pi_local_spider_subthreads


      subthreads.each do |st|
        st.join
        __join_subthreads(st)
      end
    end

    def __load_buffer(id)
      id = id.to_s
      raise "Aborting load: file name is blank" if  id.empty?
      path = project_path + id + '.spi'
      s = "# Welcome to Sonic Pi\n\n"
      if File.exist? path
        s = IO.read(path)
      end
      __replace_buffer(id, s)
    end

    def __replace_buffer(id, content)
      id = id.to_s
      content = content.to_s
      __msg_queue.push({type: "replace-buffer", buffer_id: id, val: content, line: 0, index: 0, first_line: 0})
    end

    def __replace_buffer_idx(idx, content)
      idx = idx.to_i
      content = content.to_s
      __msg_queue.push({type: "replace-buffer-idx", buffer_idx: idx, val: content, line: 0, index: 0, first_line: 0})
    end

    def __run_buffer_idx(idx)
      idx = idx.to_i
      content = content.to_s
      __msg_queue.push({type: "run-buffer-idx", buffer_idx: idx})
    end

    def __add_completion(k, text, point_line_offset=0, point=0)
      @snippets[k] = [text, point_line_offset, point]
    end

    def __snippet_completion?(text)
      text = text.rstrip
      @snippets.each do |k, v|
        if text.end_with?(k) && ((text.length == k.length)  || (text[((k.length * -1) - 1)] == " "))
          return [k, v]
        end
      end
      return nil
    end

    def __indent_lines(buf)

    end


    def __buffer_indent_lines(workspace_id, buf, start_line, finish_line, point_line, point_index)
      __buffer_complete_snippet_or_indent_lines(workspace_id, buf, start_line, finish_line, point_line, point_index, false)
    end

    def __complete_snippet_or_indent_lines(buf, start_line, finish_line, point_line, point_index, complete_snippet=true)
      orig_finish_line = finish_line
      snippet_completion = false
      buf = buf + "\n"
      buf_lines = buf.lines.to_a
      if (start_line == finish_line)
        completion_line = buf_lines[start_line].to_s.rstrip

        c = complete_snippet && __snippet_completion?(completion_line[0...point_index])
        if c
          snippet_completion = true
          completion_key, val = *c
          completion_text, point_line_offset, orig_new_point_index = *val
          new_point_index = orig_new_point_index
          orig_completion_lines = completion_text.lines.to_a
          completion_line_pre = completion_line[0...(point_index - completion_key.length)]
          completion_line_post = completion_line[point_index..-1]
          completion_text = completion_line_pre + completion_text + completion_line_post + (completion_line_post.empty? ? "" : "\n")
          completion_lines = completion_text.lines.to_a
          point_line = point_line + point_line_offset

          buf_lines = buf_lines[0...start_line] + completion_lines + buf_lines[(start_line + 1)..-1]
          new_point_line_content = buf_lines[point_line].to_s
          new_point_line_content_len = new_point_line_content.length

          if point_line_offset == 0
            if new_point_index < 0
              new_point_index = [0, completion_line_pre.length + orig_completion_lines[0].length + new_point_index + 1].max
            else
              new_point_index += completion_line_pre.length
            end
          else
            if new_point_index < 0
              new_point_index = [0, (new_point_line_content_len + new_point_index)].max
            else

              new_point_index = [new_point_index, buf_lines[point_line].to_s.length].min

            end
          end

          point_index = new_point_index
          finish_line = start_line + completion_lines.size - 1
        end
      end

      if (start_line <= point_line) && (point_line <= finish_line)
        manipulate_point = true
        # Calculate amount of whitespace at start of original line
        orig_point_line = buf_lines[point_line]
        orig_point_line_ws_len = orig_point_line[/\A */].size
        dummy_lines = false
        dummy_point_line = false

        (start_line..finish_line).each do |line_idx|
          b = buf_lines[line_idx]
          if b.match(/\A\s*\Z/)
            #line is just whitespace, put in a dummy line so it gets autoindented
            buf_lines[line_idx] = "#___sonic_pi_dummy_line___\n"
            dummy_lines = true
            dummy_point_line = true if point_line == line_idx
          end
        end
      else
        manipulate_point = false
      end

      # Beautify buffer

      beautiful = beautify_ruby_source(buf_lines.join)

      # calculate amount of whitespace at start of beautified line
      beautiful_lines = beautiful.lines.to_a
      if manipulate_point
        if dummy_point_line
          # remove dummy line and extract leading whitespace
          indented_dummy = beautiful_lines[point_line]
          indented_dummy_whitespace = indented_dummy.match(/\A(\s*)/)[1]
          beautiful_lines[point_line] = indented_dummy_whitespace + "\n"
          point_index = indented_dummy_whitespace.size
        else
          new_point_line = beautiful_lines[point_line]
          new_point_line_ws_len = new_point_line[/\A */].size

          # shift index based on how much the line was indented so the
          # cursor stays in the same place relative to the original line
          # whilst ensuring it stays within line bounds
          point_index = point_index + (new_point_line_ws_len - orig_point_line_ws_len)
          point_index = new_point_line.size - 1 if point_index > new_point_line.size
          point_index = orig_point_line_ws_len if point_index < orig_point_line_ws_len
        end

        if dummy_lines
          # remove other dummy lines
          (start_line..finish_line).each do |line_idx|
            line = beautiful_lines[line_idx]
            m = line.match(/\A(\s*)#___sonic_pi_dummy_line___/)
            beautiful_lines[line_idx] = m[1] + "\n" if m
          end
        end
      end
      indented_lines = beautiful_lines[start_line..finish_line].join
      finish_line = orig_finish_line if snippet_completion

      return {val: indented_lines, start_line: start_line, finish_line: finish_line, point_line: point_line, point_index: point_index}
    end

    def __buffer_complete_snippet_or_indent_lines(workspace_id, buf, start_line, finish_line, point_line, point_index, complete_snippet=true)
      id = workspace_id.to_s
      res = __complete_snippet_or_indent_lines(buf, start_line, finish_line, point_line, point_index, true)
      __msg_queue.push(res.merge({type: "replace-lines", buffer_id: id}))
    end

    def __buffer_newline_and_indent(workspace_id, buf, point_line, point_index, first_line)
        id = workspace_id.to_s
      lines =  buf.lines.to_a
      if lines == []
        lines = ["\n"]
      else
        if lines[point_line]
          lines[point_line].insert(point_index , "\n")
        else
          lines[point_line] = "\n"
        end
      end

      buf = lines.join

      __buffer_beautify(id, buf, point_line + 1, 0, first_line)
    end


    def __toggle_comment(workspace_id, buf, start_line, finish_line, point_line, point_index)
      id = workspace_id.to_s
      buf_lines = buf.lines.to_a
      # Check to see if we need to comment or uncomment:
      # If all lines in selection start with a # (after whitespace)
      # or are whitespace we need to uncomment.
      # Otherwise comment
      lines = buf_lines[start_line..finish_line]

      if(lines.all?{|el| el.match(/^\s*#.*?/) || el.match(/^\s*$/)})
        # need to uncomment ##| style comments
        lines = lines.map do |l|
          m = l.match(/^(\s*)#[#\| ]*(.*)/)
          if m
            m[1] + m[2] + "\n"
          else
            l
          end
        end
      else
        # need to comment
        # find shortest amount of whitespace at beginning of line
        ws = Float::INFINITY
        lines.each do |l|
          m = l.match(/^(\s*).*/)
          ws = m[1].size if m && m[1].size < ws unless l.match(/^(\s*)$/)
        end

        lines.each do |l|
          l[ws] = "##| #{l[ws]}" unless l.match(/^(\s*)$/)
        end
      end

      __msg_queue.push({type: "replace-lines", buffer_id: id, val: lines.join, start_line: start_line, finish_line: finish_line, point_line: point_line, point_index: point_index})
    end

    def __buffer_beautify(id, buf, line, index, first_line)
      id = id.to_s
      buf = buf + "\n"
      buf_lines = buf.lines.to_a
      buf = buf_lines.inspect
      buf_lines = buf_lines.map! do |l|
        if l.match(/^\s*$/)
          "_____sonic_pi_tmp_insert_____\n"
        else
          l
        end
      end
      buf = buf_lines.join
      ## ensure point isn't beyond buffer
      max_buf_idx = buf_lines.size - 1
      line  = max_buf_idx if line > max_buf_idx


      # Calculate amount of whitespace at start of original line
      prev_line = buf_lines[line]
      prev_ws_len = prev_line[/\A */].size

      # Beautify buffer

      beautiful = beautify_ruby_source(buf)

      # calculate amount of whitespace at start of beautified line
      beautiful_lines = beautiful.lines.to_a
      beautiful_len = beautiful_lines.size
      beautiful_lines.map! {|l| l.slice! "_____sonic_pi_tmp_insert_____" ; l}

      post_line = beautiful_lines[line]
      post_ws_len = post_line[/\A */].size
      beautiful = beautiful_lines.join

      # shift index based on how much the line was indented so the
      # cursor stays in the same place relative to the original line
      # whilst ensuring it stays within line bounds
      index = index + (post_ws_len - prev_ws_len)
      index = post_line.size - 1 if index > post_line.size

      # adjust line number based on how many lines were removed as a
      # result of the whitespace stripping
      post_lstrip_len = beautiful.lines.to_a.size
      line = line - (beautiful_len - post_lstrip_len)
      line = 0 if line < 0
      beautiful.chomp!
      __msg_queue.push({type: "replace-buffer", buffer_id: id, val: beautiful, line: line, index: index, first_line: first_line})
    end

    def __save_buffer(id, content)
      @save_queue << [id, content]
    end

    def __disable_update_checker
      @settings.set(:no_update_checking, true)
    end

    def __enable_update_checker
      @settings.del(:no_update_checking)
    end

    def __run_tests
      path_to_test_folder = File.join(server_path, "sonicpi", "test")
      test_paths = Dir.glob(File.join(path_to_test_folder, "test_*.rb"))
      __info "Running tests, hold tight..."
      #output = []
      test_paths.each do |test_path|
        __info "Running #{File.basename(test_path)}"
        __info `#{ruby_path} #{test_path} 2>&1`
      end
      #__error(Exception.new(output.join("\n")))
    end

    def __set_default_system_thread_locals!
      __system_thread_locals.set_local :sonic_pi_local_spider_subthread_mutex, Mutex.new
      __system_thread_locals.set_local :sonic_pi_local_spider_no_kill_mutex, Mutex.new
      __system_thread_locals.set_local :sonic_pi_local_spider_subthreads, Set.new
      __system_thread_locals.set_local :sonic_pi_local_spider_delayed_blocks, []
      __system_thread_locals.set_local :sonic_pi_local_spider_delayed_messages, []
      __system_thread_locals.set_local :sonic_pi_local_control_deltas, {}
      __system_thread_locals.set :sonic_pi_spider_thread, true
    end

    def __set_default_user_thread_locals!
      __thread_locals.set :sonic_pi_spider_arg_bpm_scaling, true
      __system_thread_locals.set :sonic_pi_spider_sleep_mul, 1.0
      __thread_locals.set :sonic_pi_spider_new_thread_random_gen_idx, 0
      __system_thread_locals.set(:sonic_pi_spider_thread_priority, 0)
    end

    def __spider_eval(code, info={})
      id = @job_counter.next

      silent = info.fetch(:silent, false)

      # skip __nosave lines for error reporting
      firstline = 1
      firstline -= code.lines.to_a.take_while{|l| l.include? "#__nosave__"}.count
      start_t_prom = Promise.new
      info[:workspace] = 'eval' unless info[:workspace]

      info[:workspace].freeze
      info.freeze
      job = Thread.new do
        Thread.current.priority = 20
        begin
          num_running_jobs = reg_job(id, Thread.current)
          __system_thread_locals.set_local :sonic_pi_local_thread_group, "job-#{id}"
          __system_thread_locals.set_local :sonic_pi_spider_thread_id_path, ThreadId.new(id)
          __system_thread_locals.set_local :sonic_pi_spider_num_threads_spawned, 0
          __system_thread_locals.set :sonic_pi_spider_job_id, id
          __system_thread_locals.set :sonic_pi_spider_silent, silent
          __system_thread_locals.set :sonic_pi_spider_job_info, info

          __set_default_system_thread_locals!
          __set_default_user_thread_locals!
          __msg_queue.push({type: :job, jobid: id, action: :start, jobinfo: info})
          @life_hooks.init(id, {:thread => Thread.current})
          now = Time.now.freeze
          start_t_prom.deliver! now
          __system_thread_locals.set :sonic_pi_spider_time, now
          __system_thread_locals.set :sonic_pi_spider_start_time, now
          __system_thread_locals.set :sonic_pi_spider_beat, 0
          if num_running_jobs == 1
            @global_start_time = now
            # Force a GC collection before we start making music!
            GC.start
          end
          __info "Starting run #{id}" unless silent
          code = PreParser.preparse(code, SonicPi::Lang::Core.vec_fns)
          code = "in_thread seed: 0 do\n" + code + "\nend"
          firstline -=1
          eval(code, nil, info[:workspace], firstline)
          __schedule_delayed_blocks_and_messages!
        rescue Stop => e
          __no_kill_block do
            __info("Stopping Run #{id}") unless silent
          end
        rescue SyntaxError => e
          __no_kill_block do
            _, line, message = *e.message.match(/\A.*:([0-9]+): (.*)/)
            error_line = ""
            if line
              line = line.to_i

              # TODO: Remove this hack when we have projects
              w = info[:workspace]
              w = normalise_buffer_name(w)
              w = "buffer #{w}"
              # TODO: end of hack

              err_msg = "[#{w}, line #{line}] \n #{message}"
              error_line = code.lines.to_a[line - firstline] ||  ""
            else
              line = -1
              err_msg = "\n #{e.message}"
            end
            __msg_queue.push({type: :job, jobid: id, action: :completed, jobinfo: info})
            __msg_queue.push({type: :syntax_error, val: err_msg, error_line: error_line , jobid: id  , jobinfo: info, line: line})
            __info("Syntax error in run #{id}. Code ignored.")
          end
        rescue Exception => e
          __schedule_delayed_blocks_and_messages!
          __no_kill_block do
            __info("Aborted Run #{id}")
            __error(e)
            __msg_queue.push({type: :job, jobid: id, action: :completed, jobinfo: info})
          end
        end
      end
      @user_jobs.add_job(id, job, info)

      Thread.new do
        Thread.current.priority = -10
        __system_thread_locals.set_local(:sonic_pi_local_thread_group, "job-#{id}-GC")
        job.join
        __join_subthreads(job)

        # wait until all synths are dead
        @life_hooks.completed(id)
        start_t = start_t_prom.get
        @life_hooks.exit(id, {:start_t => start_t})
        deregister_job_and_return_subthreads(id)
        @user_jobs.job_completed(id)
        Kernel.sleep default_sched_ahead_time
        __info "Completed run #{id}" unless silent
        unless @user_jobs.any_jobs_running?
          __info "All runs completed" unless silent
          __msg_queue.push({type: :all_jobs_completed})
          @life_hooks.all_completed(silent)
        end

        __msg_queue.push({type: :job, jobid: id, action: :completed, jobinfo: info})

      end
    end

    def __exit
      log "Runtime - shutting down..."
      @event_t.kill
      log "Runtime - stopping all jobs..."
      __stop_jobs
      __msg_queue.push({:type => :exit, :jobid => __current_job_id, :jobinfo => __current_job_info})
      log "Runtime - shutdown completed."
    end

    def __describe_threads
      names = Thread.list.map{|t| __system_thread_locals(t).get(:sonic_pi_local_thread_group)}
      __info "n-threads: #{Thread.list.size}, names: #{names}, s: #{names.size}"
    end

    def __current_tracker
      tracker = __system_thread_locals.get(:sonic_pi_local_tracker)
      if tracker
        return tracker
      else
        tracker = SynthTracker.new
        __system_thread_locals.set_local(:sonic_pi_local_tracker, tracker)
        return tracker
      end
    end

    def __current_subthreads(t = Thread.current)
      subthreads = __system_thread_locals(t).get(:sonic_pi_local_spider_subthreads)
      if subthreads
        return subthreads
      else
        subthreads = Set.new
        subthreads = __system_thread_locals(t).set_local(:sonic_pi_local_spider_subthreads, subthreads)
      end
    end

    def __current_thread_id
      __system_thread_locals.get :sonic_pi_spider_thread_id_path
    end

    def __msg_queue
      @msg_queue
    end

    def __gui_cue_log_idxs
      @gui_cue_log_idxs
    end

    def __in_thread(*opts, &block)
        args_h = resolve_synth_opts_hash_or_array(opts)
        name = args_h[:name]
        delay = args_h[:delay]
        sync_sym = args_h[:sync]
        sync_bpm_sym = args_h[:sync_bpm]
        sync_sym = nil if sync_bpm_sym

        raise ArgumentError, "in_thread's delay: opt must be a number, got #{delay.inspect}" if delay && !delay.is_a?(Numeric)

        parent_t = Thread.current

        # Get copy of thread locals whilst we're sure they're not being modified
        # as we're in the thread parent_t

        job_id = __current_job_id
        reg_with_parent_completed = Promise.new
        subthread_added_prom = Promise.new

        if args_h[:seed]
          new_rand_seed = args_h[:seed]
        else
          new_thread_gen_idx = __thread_locals.get :sonic_pi_spider_new_thread_random_gen_idx
          new_rand_seed = SonicPi::Core::SPRand.rand!(441000, new_thread_gen_idx)
          __thread_locals.set :sonic_pi_spider_new_thread_random_gen_idx, new_thread_gen_idx + 1
        end

        new_tls = SonicPi::Core::ThreadLocal.new(__thread_locals, SonicPi::Core::SPRand.tl_seed_map(new_rand_seed + SonicPi::Core::SPRand.get_seed, 0))

        new_system_tls = SonicPi::Core::ThreadLocal.new(__system_thread_locals)


        n_threads_spawned = __system_thread_locals.get :sonic_pi_spider_num_threads_spawned
        __system_thread_locals.set_local :sonic_pi_spider_num_threads_spawned, n_threads_spawned + 1
        thread_id_path = __system_thread_locals.get :sonic_pi_spider_thread_id_path
        new_thread_id_path = thread_id_path << n_threads_spawned

        # Create the new thread
        t = nil
        __no_kill_block do


          t = Thread.new do
            # Copy thread locals across from parent thread to this new thread
            __thread_locals_reset!(new_tls)
            __system_thread_locals_reset!(new_system_tls)
            __system_thread_locals.set_local(:sonic_pi_local_thread_group, :job_subthread)
            __system_thread_locals.set_local(:sonic_pi_spider_num_threads_spawned, 0)
            __system_thread_locals.set_local(:sonic_pi_spider_thread_id_path, new_thread_id_path)
            __system_thread_locals.set_local(:sonic_pi_spider_thread_delta, 0)

            # Thread locals used for waiting for threads (essential for Time State's determinism)
            __system_thread_locals.set_local(:sonic_pi_spider_state_waiters, [])
            __system_thread_locals.set_local(:sonic_pi_spider_time_change, Mutex.new)

            main_t = Thread.current
            main_t.priority = 10

            # Thread GC - will wait for main thread to finish and then execute
            Thread.new do
              if name
                __system_thread_locals.set_local(:sonic_pi_local_thread_group, "in_thread_join_#{name}".freeze)
              else
                __system_thread_locals.set_local(:sonic_pi_local_thread_group, :in_thread_join)
              end
              Thread.current.priority = -10

              main_t.join

              # remove any un-matched event matchers created
              # by the thread we're currently cleaning up
              # after:
              @event_history.prune(new_thread_id_path)

              # Remove any time state waiters
              __system_thread_locals(main_t).get(:sonic_pi_spider_time_change).synchronize do
                __system_thread_locals(main_t).get(:sonic_pi_spider_state_waiters).delete_if do |w|
                  w[:prom].deliver! true
                end
              end


              # wait for all subthreads to finish before removing self from
              # the parent subthread tree
              __join_subthreads(main_t)

              __system_thread_locals(parent_t).get(:sonic_pi_local_spider_subthread_mutex).synchronize do
                __current_subthreads.delete(main_t)
              end
            end

            __system_thread_locals.set_local :sonic_pi_local_spider_users_thread_name, name if name

            __system_thread_locals.set_local :sonic_pi_local_spider_delayed_blocks, []
            __system_thread_locals.set_local :sonic_pi_local_spider_delayed_messages, []
            __system_thread_locals.set_local :sonic_pi_local_control_deltas, {}


            # Reset subthreads thread local to the empty set. This shouldn't
            # be inherited from the parent thread.
            __system_thread_locals.set_local :sonic_pi_local_spider_subthreads, Set.new

            # Give new thread a new subthread mutex
            __system_thread_locals.set_local :sonic_pi_local_spider_subthread_mutex, Mutex.new

            # Give new thread a new no_kill mutex This reduces contention
            # over the alternative of a global no_kill mutex.  Killing a Run
            # then essentially turns into waiting for each no_kill mutext for
            # every sub-in_thread before killing them.
            __system_thread_locals.set_local :sonic_pi_local_spider_no_kill_mutex, Mutex.new


            # Wait for parent to deliver promise. Throws an exception if
            # parent dies before the promise is delivered, thus stopping
            # this thread from continually waiting for forgotten promises...
            wait_for_parent_thread!(parent_t, reg_with_parent_completed)

            # Attempt to associate the current thread with job with
            # job_id. This will kill the current thread if job is no
            # longer running or if there already exists a name thread
            # with this same name
            added_subthread_prom = Promise.new
            main_in_thread = Thread.current

            subthread_added_deliver_thread = Thread.new do
              # The following line after this block will attempt to add
              # the thread to the list of job subthreads but that action
              # will potentially terminate the current thread if the
              # main job isn't running or if there already exists a
              # similarly named thread. Therefore we attempt to join on
              # its death here to ensure we deliver the subthread_added
              # promise both in the case where the main thread dies as
              # explained above or in the case where it doesn't die :-)
              main_in_thread.join
              subthread_added_prom.deliver! true
            end

            job_subthread_add(job_id, main_in_thread, name)

            # Thread didn't die! Deliver promise and tidy up delivery
            # thread
            subthread_added_prom.deliver! true
            subthread_added_deliver_thread.kill


            # Actually run the thread code specified by the user!
            begin
              sleep delay if delay
              sync sync_sym if sync_sym
              sync_bpm sync_bpm_sym if sync_bpm_sym

              block.call
              # ensure delayed jobs and messages are honoured for this
              # thread:
              __schedule_delayed_blocks_and_messages!
            rescue Stop => e
              if name
                __info("Stopping thread #{name.inspect}")
              else
                __delayed_message("Stopped internal thread")
              end
              __schedule_delayed_blocks_and_messages!
              __current_tracker.get
              job_subthread_rm(job_id, Thread.current)
              #raise e
            rescue Exception => e
              if name
                __error e, "Thread death +--> #{name.inspect}"
              else
                __error e, "Thread death!"
              end

              # Wait for any trackers by blocking on all promises until
              # All have been delivered
              __current_tracker.get
              __schedule_delayed_blocks_and_messages!
              job_subthread_rm(job_id, Thread.current)
              #raise e
            else

              # Wait for any trackers by blocking on all promises until
              # All have been delivered
              __current_tracker.get

              # Disassociate thread with job as it has now finished
              job_subthread_rm(job_id, Thread.current)
            end
          end

          # Whilst we know that the new thread is waiting on the promise to
          # be delivered, we can now add it to our list of subthreads. Using
          # the promise means that we can be assured that killing this
          # current thread won't create a zombie child thread as the child
          # thread will only continue exiting after it has been sucessfully
          # registered.

          __system_thread_locals(parent_t).get(:sonic_pi_local_spider_subthread_mutex).synchronize do
            subthreads = __system_thread_locals(parent_t).get :sonic_pi_local_spider_subthreads
            subthreads.add(t)
          end

          # Allow the subthread to continue running
          reg_with_parent_completed.deliver! true

          # Wait for thread to be registered before continuing...
          subthread_added_prom.get
        end

        # Return subthread
        t
      end



    private

    def reg_job(job_id, t)
      num_current_jobs = 0
      @job_subthread_mutex.synchronize do
        @job_subthreads[job_id] = Set.new
        @job_main_threads[job_id] = t
        num_current_jobs = @job_main_threads.keys.size
      end
      num_current_jobs
    end

    def job_subthread_add_unmutexed(job_id, t, name=nil)
      #todo only add subthread if name isn't registered yet

      unless @job_subthreads[job_id]
        t.kill
        job_subthread_rm_unmutexed(job_id, t)
        return false
      end

      if name
        if @named_subthreads[name]
          #Don't delay following message, as this method is used for worker thread impl.
          __info "Thread #{name.inspect} exists: skipping creation"

          t.kill
          job_subthread_rm_unmutexed(job_id, t)
          return false
        else
          # register this name with the corresponding job id and also
          # store it in a thread local
          @named_subthreads[name] = SThread.new(name, job_id, t)
          __system_thread_locals(t).set_local :sonic_pi_local_spider_subthread_name, name
        end
      end

      threads = @job_subthreads[job_id]
      @job_subthreads[job_id] = threads.add(t)
    end


    def job_subthread_add(job_id, t, name=nil)
      #todo only add subthread if name isn't registered yet
      @job_subthread_mutex.synchronize do
        job_subthread_add_unmutexed(job_id, t, name)
      end
    end

    def job_subthread_rm_unmutexed(job_id, t)
      threads = @job_subthreads[job_id]
      threads.delete(t) if threads
      subthread_name = __system_thread_locals(t).get(:sonic_pi_local_spider_subthread_name)
      @named_subthreads.delete(subthread_name) if subthread_name
    end

    def job_subthread_rm(job_id, t)
      @job_subthread_mutex.synchronize do
        job_subthread_rm_unmutexed(job_id, t)
      end
    end

    def deregister_job_and_return_subthreads(job_id)
      threads = @job_subthread_mutex.synchronize do
        threads = @job_subthreads[job_id]
        @job_subthreads.delete(job_id)
        @named_subthreads.delete_if{|k,v| v.job_id == job_id}
        @job_main_threads.delete(job_id)
        threads
      end
    end

    def job_subthreads_kill(job_id)
      threads = deregister_job_and_return_subthreads(job_id)
      return :no_threads_to_kill unless threads

      ## It's safe to kill these threads outside of a mutex as now that
      ## the job id is no longer registered with @job_subthreads, new
      ## threads created by this job will be instantly killed by
      ## job_subthreadd_add

      threads.each do |t|
        __no_kill_block t do
          t.kill
        end
      end
    end

    # Synchronise on the promise. This means that we block this new
    # thread until we're absolutly sure it's been registered with the
    # parent thread as a thread local var. If the promise isn't
    # delivered within 10s, we assume the parent thread has been killed
    # so we abort running this thread.
    def wait_for_parent_thread!(parent_t, prom)
      begin
        prom.get(10)
      rescue
        raise "Parent thread died!" unless parent_t.alive?
        wait_for_parent_thread!(parent_t, prom)
      end
    end

    def filter_for_save(s)
      s.lines.to_a.reject{|l| l.include? "#__nosave__"}.join
    end

    def sthread(name)
      st = @named_subthreads[name]
      st.thread if st
    end
    def beautify_ruby_source(source)
      source = source << "\n" unless source.end_with? "\n"
      RBeautify.beautify_string :ruby, source
    end

    def normalise_buffer_name(name)
      norm = case name
             when "workspace_zero"
               "0"
             when "workspace_one"
               "1"
             when "workspace_two"
               "2"
             when "workspace_three"
               "3"
             when "workspace_four"
               "4"
             when "workspace_five"
               "5"
             when "workspace_six"
               "6"
             when "workspace_seven"
               "7"
             when "workspace_eight"
               "8"
             when "workspace_nine"
               "9"
             else
               name
             end
      return norm
    end



  end

  class Runtime

    include Util
    include ActiveSupport
    include RuntimeMethods

    def initialize(ports, msg_queue, user_methods)
      @ports = ports
      @git_hash = __extract_git_hash
      gh_short = @git_hash ? "-#{@git_hash[0, 5]}" : ""
      @settings = Config::Settings.new(user_settings_path)
      @version = Version.new(3, 2, 0, "dev#{gh_short}")
      @server_version = __server_version
      @life_hooks = LifeCycleHooks.new
      @msg_queue = msg_queue
      @cue_events = IncomingEvents.new
      @job_counter = Counter.new(-1) # Start counting jobs from 0
      @job_subthreads = {}
      @job_main_threads = {}
      @named_subthreads = {}
      @job_subthread_mutex = Mutex.new
      @user_jobs = Jobs.new
      @user_methods = user_methods
      @global_start_time = Time.now
      @session_id = SecureRandom.uuid
      @snippets = {}
      @osc_cues_port = ports[:osc_cues_port]
      @osc_router_port = ports[:erlang_port]
      @osc_server = SonicPi::OSC::UDPServer.new(@osc_cues_port, open: false)
      @system_state = EventHistory.new(@job_subthreads, @job_subthread_mutex)
      @user_state = EventHistory.new(@job_subthreads, @job_subthread_mutex)
      @event_history = EventHistory.new(@job_subthreads, @job_subthread_mutex)
      @system_init_thread_id = ThreadId.new(-1)
      osc_cue_server_thread_id = ThreadId.new(-2)
      @system_state.set 0, 0, osc_cue_server_thread_id, 0, 0, 60, :sched_ahead_time, default_sched_ahead_time
      @gui_cue_log_idxs = Counter.new
      @osc_cue_server_mutex = Mutex.new
      @register_cue_event_lambda = lambda do |t, p, i, d, b, m, address, args, sched_ahead_time=0|

        sym = nil
        address, sym = *address if address.is_a?(Array)

        gui_log_id = @gui_cue_log_idxs.next
        args = args.__sp_make_thread_safe
        address = address.to_s.freeze
        @event_history.set(t, p, i, d, b, m, address, args)
        @cue_events.async_event("/spider_thread_sync/#{address}", {
                                  :time => t,
                                  :cue_splat_map_or_arr => args,
                                  :cue => address })

        sched_ahead_sync_t = t + sched_ahead_time
        sleep_time = sched_ahead_sync_t - Time.now
        if sleep_time > 0
          Thread.new do
            Kernel.sleep(sleep_time) if sleep_time > 0
            __msg_queue.push({:type => :incoming, :time => t.to_s, :id => gui_log_id, :address => address, :args => args.inspect})
            __msg_queue.push({:type => :incoming, :time => t.to_s, :id => gui_log_id, :address => sym, :args => args.inspect}) if sym
          end
        else
          __msg_queue.push({:type => :incoming, :time => t.to_s, :id => gui_log_id, :address => address, :args => args.inspect})
          __msg_queue.push({:type => :incoming, :time => t.to_s, :id => gui_log_id, :address => sym, :args => args.inspect}) if sym
        end
      end

      # TODO Add support for TCP

      @gui_heartbeats = {}
      @gui_last_heartbeat = nil
      begin
        @gitsave = GitSave.new(project_path)
      rescue
        @gitsave = nil
      end
      @save_queue = SizedQueue.new(20)

      @event_t = Thread.new do
        __system_thread_locals.set_local(:sonic_pi_local_thread_group, :event_loop)
        Kernel.loop do
          event = @event_queue.pop
          __handle_event event
        end
      end

      @save_t = Thread.new do
        __system_thread_locals.set_local(:sonic_pi_local_thread_group, :save_loop)
        Kernel.loop do
          event = @save_queue.pop
          id, content = *event
          filename = id + '.spi'
          path = project_path + "/" + filename
          content = filter_for_save(content)
          begin
            File.open(path, 'w') {|f| f.write(content) }
            @gitsave.save!(filename, content, "#{@version} -- #{@session_id} -- ")
          rescue Exception => e
            log "Exception saving buffer #{filename}:\n#{e.inspect}"
            ##TODO: remove this and ensure that git saving actually works
            ##instead of cowardly hiding the issue!
          end
        end
      end

      __info "Welcome to Sonic Pi #{version}", 1

      __info "Running on Ruby v#{RUBY_VERSION}"

      __info "Initialised Erlang OSC Scheduler"

      if safe_mode?
        __info "!!WARNING!! - file permissions issue:\n   Unable to write to folder #{home_dir} \n   Booting in SAFE MODE.\n   Buffer auto-saving is disabled, please save your work manually.", 1
      end

      log "Unable to initialise git repo at #{project_path}" unless @gitsave
      load_snippets(snippets_path, true)
    end

    def __print_boot_messages
      __info [
"Somewhere in the world
   the sun is shining
   for you right now.",
"Hello, it's lovely to see
   you again. I do hope that
   you're well.",
"Turn your head towards the sun
   and the shadows
   will fall
   behind you.",
"Remember, when live coding music
   there are no mistakes
   only opportunities to learn
   and improve.",
"The only secret to mastering
   live coding is practice.
   Lots and lots of practice.",
"When you share
   your work and ideas
   freely with others
   the whole world benefits." ].sample, 1

      __info "Let the Live Coding begin..."


      __print_version_outdated_info if @version < @server_version
      msg = @settings.get(:message) || ""
      msg = msg.strip
      __info msg unless msg.empty?
    end


  end


end
