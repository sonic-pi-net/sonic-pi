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
require 'monitor'

module SonicPi
  class Jobs

    def initialize
      # mutex needs to be reentrant so use a Monitor
      @mut = Monitor.new
      @jobs = {}
    end

    def add_job(id, job, info)
      @mut.synchronize do
        @jobs[id] = {:job => job, :info => info}
      end
    end

    def job_completed(id)
      @mut.synchronize do
        @jobs.delete id
      end
    end

    def kill_job(id)
      @mut.synchronize do
        job = @jobs.delete(id)
        if job
          __system_thread_locals(job[:job]).get(:sonic_pi_local_spider_no_kill_mutex).synchronize do
            job[:job].kill
          end
        end
      end
    end

    def running?(id)
      @mut.synchronize do
        @jobs.has_key?(id)
      end
    end

    def each_id(&block)
      @mut.synchronize do
        @jobs.keys.each(&block)
      end
    end

    def any_jobs_running?
      @mut.synchronize do
        !@jobs.empty?
      end
    end
  end
end
