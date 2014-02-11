require_relative "counter"
require_relative 'atom'
require 'thread'

require 'hamster/hash'

module SonicPi
  class Jobs

    def initialize
      @jobs_A = Atom.new(Hamster.hash)
    end

    def add_job(id, job, info)
      @jobs_A.swap! do |js|
        js.put(id, {:job => job, :info => info})
      end
    end

    def job_completed(id)
      @jobs_A.swap! do |js|
        js.delete id
      end
    end

    def kill_job(id)
      old = @jobs_A.swap_returning_old! do |js|
        js.delete id
      end

      job = old[id]
      job[:job].kill if job
    end

    def each_id(&block)
      @jobs_A.deref.keys.each(&block)
    end
  end
end
