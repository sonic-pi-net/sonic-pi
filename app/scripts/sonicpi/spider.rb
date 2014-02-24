require_relative "util"
require_relative "studio"
require_relative "incomingevents"
require_relative "counter"
require_relative "promise"
require_relative "jobs"
require_relative "mods/spmidi"
#require_relative "mods/graphics"
require_relative "mods/sound"
#require_relative "mods/feeds"
#require_relative "mods/globalkeys"

require 'thread'
require 'fileutils'
require 'set'

module SonicPi
  class Spider

    attr_reader :event_queue

    def initialize(hostname, port, msg_queue, max_concurrent_synths)
      @msg_queue = msg_queue
      @event_queue = Queue.new
      @keypress_handlers = {}
      __message "Starting..."
      @events = IncomingEvents.new
      @sync_counter = Counter.new
      @job_counter = Counter.new
      @job_subthreads = {}
      @job_subthread_mutex = Mutex.new
      @user_jobs = Jobs.new

      @event_t = Thread.new do
        loop do
          event = @event_queue.pop
          __handle_event event
        end
      end
    end

    #These includes must happen after the initialize method
    #as they may potentially redefine it to extend behaviour
    include SonicPi::Mods::SPMIDI
#    include SonicPi::Mods::Graphics
    include SonicPi::Mods::Sound
#    include SonicPi::Mods::Feeds
#    include SonicPi::Mods::GlobalKeys

    def on_keypress(&block)
      @keypress_handlers[:foo] = block
    end

    def print(output)
      __message output
    end

    def puts(output)
      __message output
    end

    def sleep(seconds)
      last = Thread.current.thread_variable_get :sonic_pi_spider_time
      now = Time.now

      new_t = last + seconds
      if now > new_t
        __message "Can't keep up..."
      else
        Kernel.sleep new_t - now
      end

      Thread.current.thread_variable_set :sonic_pi_spider_time, new_t
    end

    def sync(sync_id, val = nil)
      @events.event("/spider_thread_sync/" + sync_id.to_s, {:time => Thread.current.thread_variable_get(:sonic_pi_spider_time), :val => val})
    end

    def wait(sync_id)
      p = Promise.new
      @events.oneshot_handler("/spider_thread_sync/" + sync_id.to_s) do |payload|
        p.deliver! payload
      end
      payload = p.get
      time = payload[:time]
      val = payload[:val]
      Thread.current.thread_variable_set :sonic_pi_spider_time, time
      val
    end

    def in_thread(&block)
      cur_t = Thread.current
      job_id = __current_job_id
      t = Thread.new do
        cur_t.thread_variables.each do |v|
          Thread.current.thread_variable_set(v, cur_t.thread_variable_get(v))
        end
        block.call
        job_subthread_rm(job_id, t)
      end
      job_subthread_add(job_id, t)
      t
    end

    ## Not officially part of the API
    ## Probably should be moved somewhere else

    def __message(s)
      @msg_queue.push({:type => :message, :val => s.to_s, :jobid => __current_job_id, :jobinfo => __current_job_info})
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
      @events.event("/job-completed", {:id => j})
      @user_jobs.kill_job j
      @msg_queue.push({type: :job, jobid: j, action: :completed})
    end

    def __stop_jobs
      __message "stopping..."
      stop
      @user_jobs.each_id do |id|
        __stop_job id
      end
    end

    def __spider_eval(code, info={})
      id = @job_counter.next
      job = Thread.new do
        begin
          Thread.current.thread_variable_set :sonic_pi_spider_time, Time.now
          Thread.current.thread_variable_set :sonic_pi_spider_job_id, id
          Thread.current.thread_variable_set :sonic_pi_spider_job_info, info
          @msg_queue.push({type: :job, jobid: id, action: :start, jobinfo: info})
          eval(code)
          @events.event("/job-join", {:id => id})
          job_subthread_join(id)
          @events.event("/job-completed", {:id => id})
          # wait until all synths are dead
          @user_jobs.job_completed(id)
          @msg_queue.push({type: :job, jobid: id, action: :completed, jobinfo: info})
        rescue Exception => e
          @events.event("/job-join", {:id => id})
          @events.event("/job-completed", {:id => id})
          job_subthreads_kill(id)
          @user_jobs.job_completed(id)
          @msg_queue.push({type: :job, jobid: id, action: :completed, jobinfo: info})
          @msg_queue.push({type: :error, val: e.message, backtrace: e.backtrace, jobid: id  , jobinfo: info})
        end
      end

      @user_jobs.add_job(id, job, info)

    end

    def __exit
      __stop_jobs
      @msg_queue.push({:type => :exit, :jobid => __current_job_id, :jobinfo => __current_job_info})
      @event_t.kill

    end

    private

    def job_subthread_add(job_id, t)
      @job_subthread_mutex.synchronize do
        threads = @job_subthreads[job_id] || Set.new([])
        @job_subthreads[job_id] = threads.add(t)
      end
    end

    def job_subthread_join(job_id)
      (@job_subthreads[job_id] || []).each do |j|
        j.join
      end
    end

    def job_subthread_rm(job_id, t)
      @job_subthread_mutex.synchronize do
        threads = @job_subthreads[job_id] || Set.new([])
        threads.delete(t)
        if threads.empty?
          @job_subthreads.delete(job_id)
        else
          @job_subthreads[job_id] = threads
        end
      end
    end

    def job_subthreads_kill(job_id)
      @job_subthread_mutex.synchronize do
        threads = @job_subthreads[job_id]
        return :no_threads_to_kill unless threads

        threads.each do |t|
          t.kill
        end

        @job_subthreads.delete(job_id)
      end
    end
  end
end
