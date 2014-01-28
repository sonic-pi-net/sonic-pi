require_relative "counter"
require 'thread'

module SonicPi
  class Jobs

    def initialize
      @jobs = {}
      @mutex = Mutex.new
    end

    def add_job(id, job, info)
      @mutex.synchronize do
        @jobs[id] = {:job => job, :info => info}
      end
    end

    def job_completed(id)
      @mutex.synchronize do
        @jobs.delete id
      end
    end

    def kill_job(id)
      @mutex.synchronize do
        job = @jobs[id]
        if job
          job[:job].kill
          @jobs.delete id
        end
      end
    end
  end
end
