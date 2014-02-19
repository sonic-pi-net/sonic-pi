module SonicPi
  class RcvDispatch
    def initialize(spider, out_queue)
      @t_sem = Mutex.new
      @spider = spider
      @out_queue = out_queue
      @event_queue = @spider.event_queue
    end

    def dispatch(data)
      @t_sem.synchronize do
        cmd = data[:cmd]

        case cmd
        when "run-code"
          exec_cmd(data)
        when "stop-jobs"
          exec_stop(data)
        when "stop-job"
          exec_stop_job(data)
        when "event"
          exec_event(data)
        when "sync"
          exec_sync(data)
        when "reload"
          exec_reload
        when "exit"
          exec_exit
        else
          raise "Unknown command: #{cmd}"
        end
      end
    end

    private

    def exec_sync(data)
      @spider.__sync(data[:val], data[:result])
    end

    def exec_stop(data)
      @spider.__stop_jobs
    end

    def exec_stop_job(data)
      @spider.__stop_job(data[:val])
    end

    def exec_cmd(data)
      @spider.__spider_eval data[:val]
    end

    def exec_event(data)
      @event_queue.push data
    end

    def exec_exit
      @spider.__exit
    end

    def exec_reload
      dir = File.dirname("#{File.absolute_path(__FILE__)}")
      Dir["#{dir}/**/*.rb"].each do |d|
        load d
      end
      puts "reloaded"
    end
  end
end
