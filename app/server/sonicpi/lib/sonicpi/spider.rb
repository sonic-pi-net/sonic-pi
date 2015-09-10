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
require_relative "util"
require_relative "studio"
require_relative "incomingevents"
require_relative "counter"
require_relative "promise"
require_relative "jobs"
require_relative "synthinfo"
require_relative "mods/spmidi"
#require_relative "mods/graphics"
require_relative "mods/sound"
#require_relative "mods/feeds"
#require_relative "mods/globalkeys"
require_relative "gitsave"
require_relative "lifecyclehooks"
require_relative "version"
require_relative "sthread"
require_relative "oscval"
require_relative "version"
require_relative "config/settings"
require_relative "preparser"

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

  class Spider

    attr_reader :event_queue
    include Util
    include ActiveSupport

    def initialize(hostname, port, msg_queue, max_concurrent_synths, user_methods)
#      @git_hash = __extract_git_hash
#      gh_short = @git_hash ? "-#{@git_hash[0, 5]}" : ""
      @settings = Config::Settings.new(user_settings_path)
      @version = Version.new(2, 7, 0)
      @server_version = __server_version
      @life_hooks = LifeCycleHooks.new
      @msg_queue = msg_queue
      @event_queue = SizedQueue.new(20)
      @keypress_handlers = {}
      @events = IncomingEvents.new
      @sync_counter = Counter.new
      @job_counter = Counter.new
      @job_subthreads = {}
      @job_main_threads = {}
      @named_subthreads = {}
      @job_subthread_mutex = Mutex.new
      @user_jobs = Jobs.new
      @sync_real_sleep_time = 0.05
      @user_methods = user_methods
      @run_start_time = 0
      @session_id = SecureRandom.uuid
      @snippets = {}

      @gui_heartbeats = {}
      @gui_last_heartbeat = nil
      @gitsave = GitSave.new(project_path)

      @event_t = Thread.new do
        Thread.current.thread_variable_set(:sonic_pi_thread_group, :event_loop)
        loop do
          event = @event_queue.pop
          __handle_event event
        end
      end
      __info "Welcome to Sonic Pi"
      __info "Session #{@session_id[0..7]}"
      date = Time.now
      __info "#{date.strftime("%A")} #{date.day.ordinalize} #{date.strftime("%B, %Y")}"
      __info "%02d:%02d, %s" % [date.hour, date.min, date.zone]

      __info "#{@version} Ready..."

      __info [
"Hello, somewhere in the world
   the sun is shining
   for you right now.",
"Hello, it's lovely to see
   you again. I do hope that
   you're well.",
"Turn your head towards the sun
   and the shadows
   will fall
   behind you."].sample




      __print_version_outdated_info if @version < @server_version

      load_snippets(snippets_path, true)
    end



    ## Not officially part of the API
    ## Probably should be moved somewhere else

    def load_snippets(path=snippets_path, quiet=false)
      path = File.expand_path(path)
      Dir["#{path}/**/*.sps"].each do |p|

        lines = File.readlines(p)
        key = nil
        completion = ""
        point_line  = 0
        point_index = 0

        while (l = lines.shift) && !(l.start_with? "# --")
          res = l.match /\# ?([_a-z]+):(.*)/
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

    def __gui_heartbeat(id)
      t = Time.now
      @gui_heartbeats[id] = t
      @gui_last_heartbeat = t
    end

    def __extract_git_hash
      head_path = root_path + "/.git/HEAD"
      if File.exists? head_path
        ref = File.readlines(head_path).first
        ref_path = root_path + "/.git/" + ref[5..-1]
        ref_path = ref_path.strip
        if File.exists? ref_path
          return File.readlines(ref_path).first
        end
      end
    end

    def __update_gui_version_info_now
      v = __check_for_server_version_now
      @msg_queue.push({:type => :version, :version => @version.to_s, :version_num =>  @version.to_i, :latest_version => v.to_s, :latest_version_num => v.to_i, :last_checked => __last_update_check})
    end

    def __current_version
      @version
    end

    def __last_update_check
      Time.at(last_update = @settings.get(:last_update_check_time).to_i)
    end

    def __check_for_server_version_now(url="http://sonic-pi.net/static/info/latest_version.txt")
      begin
        params = {:uuid => global_uuid,
                  :ruby_platform => RUBY_PLATFORM,
                  :ruby_version => RUBY_VERSION,
                  :ruby_patchlevel => RUBY_PATCHLEVEL,
                  :sonic_pi_version => @version.to_s}
        uri = URI.parse(url)
        uri.query = URI.encode_www_form( params )
        response = Net::HTTP.get_response uri
        v_string = response.body
        v = Version.init_from_string(v_string)
        @settings.set(:last_update_check_time, Time.now.to_i)
        @settings.set(:last_seen_server_version, v.to_s)
        v
      rescue
        __local_cached_server_version
      end
    end

    def __server_version(url="http://sonic-pi.net/static/info/latest_version.txt")
      return Version.new(0) if @settings.get(:no_update_checking)

      # Only check for updates at most once every 2 weeks
      last_update = @settings.get(:last_update_check_time).to_i
      if  (last_update > 0) &&
          (Time.at(last_update) < Time.now)
        two_weeks_in_seconds = 60 * 60 * 24 * 14
        ts_2_weeks_later = Time.at(last_update + two_weeks_in_seconds)
        return __local_cached_server_version if Time.now < ts_2_weeks_later
      end

      __check_for_server_version_now(url)
    end

    def __local_cached_server_version
      begin
        return Version.init_from_string(@settings.get(:last_seen_server_version))
      rescue
        return Version.new(0)
      end
    end

    def __print_version_outdated_info
      __info "Your version of Sonic Pi is outdated"
      __info "The latest is #{@server_version}"
      __info "Please consider updating..."
    end


    def __no_kill_block(t = Thread.current, &block)
      return block.call if t.thread_variable_get(:sonic_pi__not_inherited__spider_in_no_kill_block)
      t.thread_variable_get(:sonic_pi_spider_no_kill_mutex).synchronize do
        t.thread_variable_set(:sonic_pi__not_inherited__spider_in_no_kill_block, true)
        r = block.call
        t.thread_variable_set(:sonic_pi__not_inherited__spider_in_no_kill_block, false)
        r
      end
    end

    def __info(s)
      @msg_queue.push({:type => :info, :val => s.to_s})
    end

    def __multi_message(m)
      @msg_queue.push({:type => :multi_message, :val => m, :jobid => __current_job_id, :jobinfo => __current_job_info, :runtime => __current_local_run_time.round(4), :thread_name => __current_thread_name})
    end

    def __delayed(&block)
      raise "Can only use __delayed in a job thread" unless __current_job_id
      delayed_blocks = Thread.current.thread_variable_get :sonic_pi_spider_delayed_blocks
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

    def __schedule_delayed_blocks_and_messages!
      delayed_messages = Thread.current.thread_variable_get :sonic_pi_spider_delayed_messages
      delayed_blocks = Thread.current.thread_variable_get(:sonic_pi_spider_delayed_blocks) || []
      unless(delayed_messages.empty?)
        last_vt = Thread.current.thread_variable_get :sonic_pi_spider_time
        parent_t = Thread.current
        job_id = parent_t.thread_variable_get(:sonic_pi_spider_job_id)

        t = Thread.new do

          Thread.current.thread_variable_set(:sonic_pi_thread_group, :send_delayed_messages)
          Thread.current.priority = -10
          #only copy the necessary thread locals from parent
          Thread.current.thread_variable_set(:sonic_pi_spider_job_id, job_id)
          Thread.current.thread_variable_set(:sonic_pi_spider_job_info, parent_t.thread_variable_get(:sonic_pi_spider_job_info))
          Thread.current.thread_variable_set(:sonic_pi_spider_time, last_vt)
          Thread.current.thread_variable_set(:sonic_pi_spider_start_time, parent_t.thread_variable_get(:sonic_pi_spider_start_time))
          Thread.current.thread_variable_set(:sonic_pi_spider_users_thread_name, parent_t.thread_variable_get(:sonic_pi_spider_users_thread_name))
          Thread.current.thread_variable_set :sonic_pi_spider_subthread_mutex, Mutex.new
          Thread.current.thread_variable_set :sonic_pi_spider_no_kill_mutex, Mutex.new

          Thread.current.thread_variable_set(:sonic_pi_core_thread_local_counters, {})
          # Calculate the amount of time to sleep to sync us up with the
          # sched_ahead_time
          sched_ahead_sync_t = last_vt + @mod_sound_studio.sched_ahead_time
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

          __multi_message(delayed_messages)
          job_subthread_rm(job_id, Thread.current)
        end

        job_subthread_add(job_id, t)

        Thread.current.thread_variable_set :sonic_pi_spider_delayed_messages, []
      end
    end

    def __enqueue_multi_message(m_type, m)
      raise "Can only use __enqueue_multi_message in a job thread" unless __current_job_id
      delayed_messages = Thread.current.thread_variable_get :sonic_pi_spider_delayed_messages
      delayed_messages << [m_type, m]
    end

    def __extract_line_of_error(e)
      trace = e.backtrace
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
      err_msg.gsub!(/for #<SonicPiSpiderUser[a-z0-9:]+>/, '')
      res = ""
      if line != -1
        res = res + "[#{info[:workspace]}, line #{line}]"
      else
        res = res + "[#{info[:workspace]}]"
      end
      res = res + "\n" + m if m
      res = res + "\n #{err_msg}"
      @msg_queue.push({type: :error, val: res, backtrace: e.backtrace, jobid: __current_job_id, jobinfo: __current_job_info, line: line})
    end

    def __current_run_time
      Thread.current.thread_variable_get(:sonic_pi_spider_time) - @run_start_time
    end

    def __current_local_run_time
      Thread.current.thread_variable_get(:sonic_pi_spider_time) - Thread.current.thread_variable_get(:sonic_pi_spider_start_time)
    end

    def __current_thread_name
      Thread.current.thread_variable_get :sonic_pi_spider_users_thread_name || ""
    end

    def __current_job_id
      Thread.current.thread_variable_get :sonic_pi_spider_job_id
    end

    def __current_job_info
      Thread.current.thread_variable_get :sonic_pi_spider_job_info || {}
    end

    def __sync_msg_command(msg)
      id = @sync_counter.next
      prom = Promise.new
      @events.add_handler("/sync", @events.gensym("/spider")) do |payload|
        if payload[:id] == id
          prom.deliver! payload[:result]
          :remove_handler
        end
      end
      msg[:sync] = id
      msg[:jobid] = __current_job_id
      msg[:jobinfo] = __current_job_info
      @msg_queue.push msg
      prom.get
    end

    def __handle_event(e)
      case e[:type]
      when :keypress
        @keypress_handlers.values.each{|h| h.call(e[:val])}
        else
          puts "Unknown event: #{e}"
        end
    end

    def __sync(id, res)
      @events.event("/sync", {:id => id, :result => res})
    end

    def __stop_job(j)
      job_subthreads_kill(j)
      @user_jobs.kill_job j
      @life_hooks.killed(j)
      @life_hooks.exit(j)
      @msg_queue.push({type: :job, jobid: j, action: :killed})
    end

    def __stop_jobs
      __info "Stopping all runs..."
      @user_jobs.each_id do |id|
        __stop_job id
      end
    end

    def __join_subthreads(t)
      subthreads = t.thread_variable_get :sonic_pi_spider_subthreads
      subthreads.each do |st|
        st.join
        __join_subthreads(st)
      end
    end

    def __load_buffer(id)
      id = id.to_s
      raise "Aborting load: file name is blank" if  id.empty?
      path = project_path + id + '.spi'
      s = "# Welcome to Sonic Pi #{@version.to_s}\n\n"
      if File.exists? path
        s = IO.read(path)
      end
      __replace_buffer(id, s)
    end

    def __replace_buffer(id, content)
      id = id.to_s
      content = content.to_s
      @msg_queue.push({type: "replace-buffer", buffer_id: id, val: content, line: 0, index: 0, first_line: 0})
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

    def __complete_snippet_or_indent_lines(workspace_id, buf, start_line, finish_line, point_line, point_index)
      orig_finish_line = finish_line
      snippet_completion = false
      id = workspace_id.to_s
      buf = buf + "\n"
      buf_lines = buf.lines.to_a
      if (start_line == finish_line)
        completion_line = buf_lines[start_line].to_s.rstrip

        c = __snippet_completion?(completion_line[0...point_index])
        if c
          snippet_completion = true
          completion_key, val = *c
          completion_text, point_line_offset, orig_new_point_index = *val
          new_point_index = orig_new_point_index
          orig_completion_lines = completion_text.lines.to_a
          completion_line_pre = completion_line[0...(point_index - completion_key.length)]
          completion_line_post = completion_line[point_index..-1]
          completion_text = completion_line_pre + completion_text + completion_line_post
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

        if buf_lines[point_line] =~ /^\s*$/
          #line is just whitespace, put in a dummy line so it gets autoindented
          buf_lines[point_line] = "#dummy\n"
          dummy_line = true
        else
          dummy_line = false
        end
      else
        manipulate_point = false
      end

      # Beautify buffer
      beautiful = RBeautify.beautify_string :ruby, buf_lines.join

      # calculate amount of whitespace at start of beautified line
      beautiful_lines = beautiful.lines.to_a
      if manipulate_point
        if dummy_line
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
      end
      indented_lines = beautiful_lines[start_line..finish_line].join
      finish_line = orig_finish_line if snippet_completion
      @msg_queue.push({type: "replace-lines", buffer_id: id, val: indented_lines, start_line: start_line, finish_line: finish_line, point_line: point_line, point_index: point_index})

    end

    def __toggle_comment(workspace_id, buf, start_line, finish_line, point_line, point_index)
      id = workspace_id.to_s
      indented_linex = ""
      buf_lines = buf.lines.to_a
      # Check to see if we need to comment or uncomment:
      # If all lines in selection start with a # (after whitespace)
      # or are whitespace we need to uncomment.
      # Otherwise comment
      lines = buf_lines[start_line..finish_line]

      if(lines.all?{|el| el.match(/^\s*#.*/) || el.match(/^\s*$/)})
        # need to uncomment
        lines = lines.map do |l|
          m = l.match(/^(\s*)#+[ ]?(.*)/)
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
          l[ws] = "# #{l[ws]}" unless l.match(/^(\s*)$/)
        end
      end

      @msg_queue.push({type: "replace-lines", buffer_id: id, val: lines.join, start_line: start_line, finish_line: finish_line, point_line: point_line, point_index: point_index})
    end

    def __beautify_buffer(id, buf, line, index, first_line)
      id = id.to_s
      buf = buf + "\n"
      buf_lines = buf.lines.to_a

      ## ensure point isn't beyond buffer
      max_buf_idx = buf_lines.size - 1
      line  = max_buf_idx if line > max_buf_idx


      # Calculate amount of whitespace at start of original line
      prev_line = buf_lines[line]
      prev_ws_len = prev_line[/\A */].size

      # Beautify buffer
      beautiful = RBeautify.beautify_string :ruby, buf

      # calculate amount of whitespace at start of beautified line
      beautiful_lines = beautiful.lines.to_a
      beautiful_len = beautiful_lines.size
      post_line = beautiful_lines[line]
      post_ws_len = post_line[/\A */].size

      # shift index based on how much the line was indented so the
      # cursor stays in the same place relative to the original line
      # whilst ensuring it stays within line bounds
      index = index + (post_ws_len - prev_ws_len)
      index = post_line.size - 1 if index > post_line.size

      # Strip whitespace at the beginning of the buffer
      beautiful.lstrip!

      # adjust line number based on how many lines were removed as a
      # result of the whitespace stripping
      post_lstrip_len = beautiful.lines.to_a.size
      line = line - (beautiful_len - post_lstrip_len)
      line = 0 if line < 0

      # Strip whitespace from the end of the buffer
      beautiful.rstrip!
      post_rstrip_len = beautiful.lines.to_a.size

      # move point to end of buffer if whitespace stripping at end of
      # buffer put point out of bounds
      if line >= post_rstrip_len
        line = post_rstrip_len
        index = beautiful.lines.to_a.last.size
      end
      @msg_queue.push({type: "replace-buffer", buffer_id: id, val: beautiful, line: line, index: index, first_line: first_line})
    end

    def __save_buffer(id, content)
      filename = id + '.spi'
      path = project_path + "/" + filename
      content = filter_for_save(content)
      File.open(path, 'w') {|f| f.write(content) }
      begin
        @gitsave.save!(filename, content, "#{@version} -- #{@session_id} -- ")
      rescue Exception => e
        ##TODO: remove this and ensure that git saving actually works
        ##instead of cowardly hiding the issue!
      end

    end

    def __disable_update_checker
      @settings.set(:no_update_checking, true)
    end

    def __enable_update_checker
      @settings.del(:no_update_checking)
    end

    def __spider_eval(code, info={})
      id = @job_counter.next

      # skip __nosave lines for error reporting
      firstline = 1
      firstline -= code.split(/\r?\n/).count{|l| l.include? "#__nosave__"}
      start_t_prom = Promise.new
      info[:workspace] = 'eval' unless info[:workspace]
      job = Thread.new do
        Thread.current.priority = 20
        begin

          num_running_jobs = reg_job(id, Thread.current)
          Thread.current.thread_variable_set :sonic_pi_spider_thread, true
          Thread.current.thread_variable_set :sonic_pi_thread_group, "job-#{id}"
          Thread.current.thread_variable_set :sonic_pi_spider_arg_bpm_scaling, true
          Thread.current.thread_variable_set :sonic_pi_spider_sleep_mul, 1.0
          Thread.current.thread_variable_set :sonic_pi_spider_job_id, id
          Thread.current.thread_variable_set :sonic_pi_spider_job_info, info
          Thread.current.thread_variable_set :sonic_pi_spider_subthreads, Set.new
          Thread.current.thread_variable_set :sonic_pi_control_deltas, {}
          Thread.current.thread_variable_set :sonic_pi_spider_subthread_mutex, Mutex.new
          Thread.current.thread_variable_set :sonic_pi_spider_no_kill_mutex, Mutex.new
          Thread.current.thread_variable_set :sonic_pi_spider_delayed_blocks, []
          Thread.current.thread_variable_set :sonic_pi_spider_delayed_messages, []
          Thread.current.thread_variable_set :sonic_pi_spider_random_gen_seed, 0
          Thread.current.thread_variable_set :sonic_pi_spider_random_gen_idx, 0
          Thread.current.thread_variable_set :sonic_pi_spider_new_thread_random_gen_idx, 0
          @msg_queue.push({type: :job, jobid: id, action: :start, jobinfo: info})
          @life_hooks.init(id, {:thread => Thread.current})
          now = Time.now
          start_t_prom.deliver! now
          Thread.current.thread_variable_set :sonic_pi_spider_time, now
          Thread.current.thread_variable_set :sonic_pi_spider_start_time, now
          @run_start_time = now if num_running_jobs == 1
          __info "Starting run #{id}"
          code = PreParser.preparse(code)

          eval(code, nil, info[:workspace], firstline)
          __schedule_delayed_blocks_and_messages!
        rescue Stop => e
          __no_kill_block do
            __info("Stopping Run #{id}")
          end
        rescue SyntaxError => e
          __no_kill_block do
            _, line, message = *e.message.match(/\A.*:([0-9]+): (.*)/)
            error_line = ""
            if line
              line = line.to_i
              err_msg = "[#{info[:workspace]}, line #{line}] \n #{message}"
              error_line = code.lines.to_a[line + 1] ||  ""
            else
              line = -1
              err_msg = "\n #{e.message}"
            end
            @msg_queue.push({type: :job, jobid: id, action: :completed, jobinfo: info})
            @msg_queue.push({type: :syntax_error, val: err_msg, error_line: error_line , jobid: id  , jobinfo: info, line: line})
            __info("Syntax error in run #{id}. Code ignored.")
          end
        rescue Exception => e
          __no_kill_block do
            __error(e)
            @msg_queue.push({type: :job, jobid: id, action: :completed, jobinfo: info})
          end
        end
      end
      @user_jobs.add_job(id, job, info)

      Thread.new do
        Thread.current.priority = -10
        Thread.current.thread_variable_set(:sonic_pi_thread_group, "job-#{id}-GC")
        job.join
        __join_subthreads(job)


        # wait until all synths are dead

        @life_hooks.completed(id)
        start_t = start_t_prom.get
        @life_hooks.exit(id, {:start_t => start_t})
        deregister_job_and_return_subthreads(id)
        @user_jobs.job_completed(id)
        Kernel.sleep @mod_sound_studio.sched_ahead_time
        __info "Completed run #{id}"
        @msg_queue.push({type: :job, jobid: id, action: :completed, jobinfo: info})
      end
    end

    def __exit
      __stop_jobs
      @msg_queue.push({:type => :exit, :jobid => __current_job_id, :jobinfo => __current_job_info})
      @event_t.kill


    end

    def __describe_threads
      __info "n-threads: #{Thread.list.size}, names: #{Thread.list.map{|t| t.thread_variable_get(:sonic_pi_thread_group)}}"
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
          t.thread_variable_set :sonic_pi__not_inherited__spider_subthread_name, name
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
      subthread_name = t.thread_variable_get(:sonic_pi__not_inherited__spider_subthread_name)
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
      s.split(/\r?\n/).reject{|l| l.include? "#__nosave__"}.join("\n")
    end

    def sthread(name)
      st = @named_subthreads[name]
      st.thread if st
    end
  end
end
