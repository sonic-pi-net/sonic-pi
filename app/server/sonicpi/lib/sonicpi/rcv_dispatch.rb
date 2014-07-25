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
require 'active_support/core_ext/hash/indifferent_access'

module SonicPi
  class RcvDispatch
    def initialize(spider, out_queue)
      @t_sem = Mutex.new
      @spider = spider
      @out_queue = out_queue
      @event_queue = @spider.event_queue
    end

    def dispatch(data)
      data = data.with_indifferent_access
      @t_sem.synchronize do
        cmd = data["cmd"]

        case cmd
        when "run-code"
          exec_cmd(data)
        when "save-and-run-buffer"
          exec_save_and_run_buffer(data)
        when "stop-jobs"
          exec_stop
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
        when "ping"
          exec_ping(data)
        when "load-buffer"
          exec_load_buffer(data)
        when "save-buffer"
          exec_save_buffer(data)
        else
          raise "Unknown command: #{cmd}"
        end
      end
    end

    private

    def exec_sync(data)
      @spider.__sync(data["val"], data["result"])
    end

    def exec_stop
      @spider.__stop_jobs
    end

    def exec_stop_job(data)
      @spider.__stop_job(data["val"])
    end

    def exec_cmd(data)
      @spider.__spider_eval data["val"]
    end

    def exec_save_and_run_buffer(data)
      buffer_content = data["val"].to_s
      buffer_id = data["buffer_id"].to_s
      @spider.__spider_eval buffer_content
      @spider.__save_buffer(buffer_id, buffer_content)
    end

    def exec_event(data)
      @event_queue.push data
    end

    def exec_exit
      @spider.__exit
    end

    def exec_ping(data)
      @event_queue.push({type: :ack, id: data["id"]})
    end

    def exec_load_buffer(data)
      @spider.__load_buffer data["id"]
    end

    def exec_save_buffer(data)
      buffer_content = data["val"].to_s
      buffer_id = data["buffer_id"].to_s
      @spider.__save_buffer(buffer_id, buffer_content)
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
