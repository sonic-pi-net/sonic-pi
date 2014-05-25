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

require 'thread'
require 'fileutils'
require 'set'

module SonicPi
  class Spider

    attr_reader :event_queue

    def initialize(hostname, port, msg_queue, max_concurrent_synths, user_methods)
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

      @gitsave = GitSave.new(project_path)

      @event_t = Thread.new do
        Thread.current.thread_variable_set(:sonic_pi_thread_group, :event_loop)
        loop do
          event = @event_queue.pop
          __handle_event event
        end
      end
      __message "RC5-dev Ready..."
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

    def __user_message(s)
      @msg_queue.push({:type => :user_message, :val => s.to_s, :jobid => __current_job_id, :jobinfo => __current_job_info})
    end

    def __warning(s)
      @msg_queue.push({:type => :warning, :val => s.to_s, :jobid => __current_job_id, :jobinfo => __current_job_info})
    end


    def __message(s)
      @msg_queue.push({:type => :message, :val => s.to_s, :jobid => __current_job_id, :jobinfo => __current_job_info})
    end

    def __delayed(&block)
      raise "Can only use __delayed in a job thread" unless __current_job_id
      delayed_blocks = Thread.current.thread_variable_get :sonic_pi_spider_delayed_blocks
      delayed_blocks << block
    end

    def __error(s, e)
      @msg_queue.push({:type => :error, :val => s.to_s, :jobid => __current_job_id, :jobinfo => __current_job_info, :backtrace => e.backtrace})
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
      __message "Stopping job #{j}"
      job_subthreads_kill(j)
      @user_jobs.kill_job j
      @events.event("/job-completed", {:id => j})
      @msg_queue.push({type: :job, jobid: j, action: :completed})
    end

    def __stop_jobs
      __message "Stopping all jobs."
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
      @msg_queue.push({type: "replace-buffer", buffer_id: id, val: s})
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
          reg_job(id, job)
          Thread.current.thread_variable_set(:sonic_pi_thread_group, :job)
          Thread.current.thread_variable_set(:sonic_pi_spider_sleep_mul, 1)
          Thread.current.thread_variable_set :sonic_pi_spider_time, Time.now
          Thread.current.thread_variable_set :sonic_pi_spider_job_id, id
          Thread.current.thread_variable_set :sonic_pi_spider_job_info, info
          Thread.current.thread_variable_set :sonic_pi_spider_subthreads, Set.new
          Thread.current.thread_variable_set :sonic_pi_control_deltas, {}
          Thread.current.thread_variable_set :sonic_pi_spider_subthread_mutex, Mutex.new
          Thread.current.thread_variable_set :sonic_pi_spider_no_kill_mutex, Mutex.new
          Thread.current.thread_variable_set :sonic_pi_spider_delayed_blocks, []
          @msg_queue.push({type: :job, jobid: id, action: :start, jobinfo: info})
          @events.event("/job-start", {:id => id, :thread => job})
          eval(code + "\nsleep 0")
          __join_subthreads(Thread.current)
          @events.event("/job-join", {:id => id})
          # wait until all synths are dead
          @user_jobs.job_completed(id)

          @events.event("/job-completed", {:id => id, :thread => job})
          job_subthreads_kill(id)
          @msg_queue.push({type: :job, jobid: id, action: :completed, jobinfo: info})
        rescue Exception => e

          @msg_queue.push({type: :job, jobid: id, action: :completed, jobinfo: info})
          @msg_queue.push({type: :error, val: e.message, backtrace: e.backtrace, jobid: id  , jobinfo: info})
          @events.event("/job-join", {:id => id})
          @events.event("/job-completed", {:id => id, :thread => job})
          job_subthreads_kill(id)
          @user_jobs.job_completed(id)

        end
      end

      @user_jobs.add_job(id, job, info)

    end

    def __exit
      __stop_jobs
      @msg_queue.push({:type => :exit, :jobid => __current_job_id, :jobinfo => __current_job_info})
      @event_t.kill

    end

    def __describe_threads
      __message Thread.list.map{|t| t.thread_variable_get(:sonic_pi_thread_group)}
    end

    private

    def reg_job(job_id, t)
      @job_subthread_mutex.synchronize do
        @job_subthreads[job_id] = Set.new
        @job_main_threads[job_id] = t
      end
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
          __message "Skipping thread creation: thread with name #{name.inspect} already exists."
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

    def job_subthreads_kill(job_id)
      threads = @job_subthread_mutex.synchronize do
        threads = @job_subthreads[job_id]
        @job_subthreads.delete(job_id)
        @named_subthreads.delete_if{|k,v| v == job_id}
        @job_main_threads.delete(job_id)
        threads
      end

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
        prom.get_with_timeout(10, 0.1)
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
