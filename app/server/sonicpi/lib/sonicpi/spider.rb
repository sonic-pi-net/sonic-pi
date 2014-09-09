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
#require_relative "oscval"
#require_relative "oscevent"

require 'thread'
require 'fileutils'
require 'set'
require 'ruby-beautify'


module SonicPi
  class Spider

    attr_reader :event_queue

    def initialize(hostname, port, msg_queue, max_concurrent_synths, user_methods)

      @version = Version.new(2, 0, 1)

      @life_hooks = LifeCycleHooks.new
      @msg_queue = msg_queue
      @event_queue = Queue.new
      @keypress_handlers = {}
      @events = IncomingEvents.new
      @sync_counter = Counter.new
      @job_counter = Counter.new
      @job_subthreads = {}
      @job_main_threads = {}
      @named_subthreads = {}
      @job_subthread_mutex = Mutex.new
      @user_jobs = Jobs.new
      @random_generator = Random.new(0)
      @sync_real_sleep_time = 0.05
      @user_methods = user_methods
      @run_start_time = 0

      @gitsave = GitSave.new(project_path)

      @event_t = Thread.new do
        Thread.current.thread_variable_set(:sonic_pi_thread_group, :event_loop)
        loop do
          event = @event_queue.pop
          __handle_event event
        end
      end
      __info "#{@version} Ready..."
    end

    #These includes must happen after the initialize method
    #as they may potentially redefine it to extend behaviour
    include SonicPi::Mods::SPMIDI
#    include SonicPi::Mods::Graphics
    include SonicPi::Mods::Sound
#    include SonicPi::Mods::Feeds
    #    include SonicPi::Mods::GlobalKeys

    ## Not officially part of the API
    ## Probably should be moved somewhere else

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

    def __schedule_delayed_blocks
      delayed_blocks = Thread.current.thread_variable_get :sonic_pi_spider_delayed_blocks
      unless(delayed_blocks.empty?)
        last_vt = Thread.current.thread_variable_get :sonic_pi_spider_time
        parent_t = Thread.current
        parent_t_vars = {}
        parent_t.thread_variables.each do |v|
          parent_t_vars[v] = parent_t.thread_variable_get(v)
        end
        p = Promise.new

        pause_then_run_blocks_and_msgs = lambda do
          # Give new thread a new subthread mutex
          Thread.current.thread_variable_set :sonic_pi_spider_subthread_mutex, Mutex.new
          Thread.current.thread_variable_set :sonic_pi_spider_no_kill_mutex, Mutex.new
          parent_t_vars.each do |k,v|
            Thread.current.thread_variable_set(k, v)
          end
          Thread.current.thread_variable_set(:sonic_pi_thread_group, :execute_delayed_blocks)
          p.get
          # Calculate the amount of time to sleep to sync us up with the
          # sched_ahead_time
          sched_ahead_sync_t = last_vt + @mod_sound_studio.sched_ahead_time
          sleep_time = sched_ahead_sync_t - Time.now
          Kernel.sleep(sleep_time) if sleep_time > 0
          #We're now in sync with the sched_ahead time

          delayed_blocks.each {|b| b.call}
          job_subthread_rm(__current_job_id, t)
        end

        @job_subthread_mutex.synchronize do
          t = Thread.new do
            Thread.current.thread_variable_set(:sonic_pi_thread_group, :scsynth_external_booter)
            pause_then_run_blocks_and_msgs.call
          end
          job_subthread_add_unmutexed(__current_job_id, t)
        end
        p.deliver! true

        Thread.current.thread_variable_set :sonic_pi_spider_delayed_blocks, []
      end
    end

    def __schedule_messages
      delayed_messages = Thread.current.thread_variable_get :sonic_pi_spider_delayed_messages
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


          # Calculate the amount of time to sleep to sync us up with the
          # sched_ahead_time
          sched_ahead_sync_t = last_vt + @mod_sound_studio.sched_ahead_time
          sleep_time = sched_ahead_sync_t - Time.now
          Kernel.sleep(sleep_time) if sleep_time > 0
          #We're now in sync with the sched_ahead time
          __multi_message(delayed_messages)
          job_subthread_rm(job_id, Thread.current)
        end

        job_subthread_add(job_id, t)

        Thread.current.thread_variable_set :sonic_pi_spider_delayed_messages, []
      end
    end

    def __schedule_delayed_blocks_and_messages!
      __schedule_delayed_blocks
      __schedule_messages
    end

    def __enqueue_multi_message(m_type, m)
      raise "Can only use __enqueue_multi_message in a job thread" unless __current_job_id
      delayed_messages = Thread.current.thread_variable_get :sonic_pi_spider_delayed_messages
      delayed_messages << [m_type, m]
    end

    def __error(s, e)
      @msg_queue.push({:type => :error, :val => s.to_s, :jobid => __current_job_id, :jobinfo => __current_job_info, :backtrace => e.backtrace})
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
      Thread.current.thread_variable_get :sonic_pi_spider_job_info
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
      s = "# Welcome to Sonic Pi v2.0"
      if File.exists? path
        s = IO.read(path)
      end
      __replace_buffer(id, s)
    end

    def __replace_buffer(id, content)
      id = id.to_s
      content = content.to_s
      @msg_queue.push({type: "replace-buffer", buffer_id: id, val: content})
    end

    def __beautify_buffer(id, buf)
      id = id.to_s
      beautiful = RBeautify.beautify_string :ruby, buf
      @msg_queue.push({type: "replace-buffer", buffer_id: id, val: beautiful})
    end

    def __save_buffer(id, content)
      filename = id + '.spi'
      path = project_path + "/" + filename
      content = filter_for_save(content)
      File.open(path, 'w') {|f| f.write(content) }
      @gitsave.save!(filename, content)
    end

    def __spider_eval(code, info={})
      id = @job_counter.next
      job = Thread.new do
        Thread.current.priority = 10
        begin
          num_running_jobs = reg_job(id, Thread.current)
          Thread.current.thread_variable_set :sonic_pi_thread_group, "job-#{id}"
          Thread.current.thread_variable_set :sonic_pi_spider_arg_bpm_scaling, true
          Thread.current.thread_variable_set :sonic_pi_spider_sleep_mul, 1
          Thread.current.thread_variable_set :sonic_pi_spider_job_id, id
          Thread.current.thread_variable_set :sonic_pi_spider_job_info, info
          Thread.current.thread_variable_set :sonic_pi_spider_subthreads, Set.new
          Thread.current.thread_variable_set :sonic_pi_control_deltas, {}
          Thread.current.thread_variable_set :sonic_pi_spider_subthread_mutex, Mutex.new
          Thread.current.thread_variable_set :sonic_pi_spider_no_kill_mutex, Mutex.new
          Thread.current.thread_variable_set :sonic_pi_spider_delayed_blocks, []
          Thread.current.thread_variable_set :sonic_pi_spider_delayed_messages, []
          Thread.current.thread_variable_set :sonic_pi_spider_random_generator, Random.new(0)
          @msg_queue.push({type: :job, jobid: id, action: :start, jobinfo: info})
          @life_hooks.init(id, {:thread => Thread.current})
          now = Time.now
          Thread.current.thread_variable_set :sonic_pi_spider_time, now
          Thread.current.thread_variable_set :sonic_pi_spider_start_time, now
          @run_start_time = now if num_running_jobs == 1
          __info "Starting run #{id}"
          eval(code)
          __schedule_delayed_blocks_and_messages!
        rescue Exception => e
          __no_kill_block do
            @msg_queue.push({type: :job, jobid: id, action: :completed, jobinfo: info})
            @msg_queue.push({type: :error, val: e.message, backtrace: e.backtrace, jobid: id  , jobinfo: info})

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

        @life_hooks.exit(id)

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
          @named_subthreads[name] = job_id
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
        @named_subthreads.delete_if{|k,v| v == job_id}
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
  end
end
