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
      Thread.new do
        loop do
          event = @event_queue.pop
          __handle_event event
        end
      end

      @user_jobs = Jobs.new
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

    def __stop_jobs
      __message "stopping..."
      stop
    end

    def __spider_eval(code, info={})
      id = @job_counter.next
      job = Thread.new do
        begin
          Thread.current.thread_variable_set :sonic_pi_spider_job_id, id
          Thread.current.thread_variable_set :sonic_pi_spider_job_info, info
          @msg_queue.push({type: :job, jobid: id, action: :start, jobinfo: info})
          eval(code)
          @user_jobs.job_completed(id)
          @msg_queue.push({type: :job, jobid: id, action: :completed, jobinfo: info})
        rescue Exception => e
          @msg_queue.push({type: :job, jobid: id, action: :completed, jobinfo: info})
          @msg_queue.push({type: :error, val: e.message, backtrace: e.backtrace, jobid: id  , jobinfo: info})
        end
      end

      @user_jobs.add_job(id, job, info)

    end
  end
end
