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

module SonicPi
  module SpiderAPI
    def define(name, &block)
      raise "define must be called with a code block" unless block

      @user_methods.send(:define_method, name, &block)
    end

    def on_keypress(&block)
      @keypress_handlers[:foo] = block
    end

    def print(output)
      __message output
    end

    def puts(output)
      __message output
    end

    def rand(limit=1.0)
      @random_generator.rand(limit)
    end

    def rrand(limit=1.0, limit2=0)
      range = (limit - limit2).abs
      r = @random_generator.rand(range.to_f)
      smallest = [limit, limit2].min
      r + smallest
    end

    def rrand_i(limit=1.0, limit2=0)
      range = (limit - limit2).abs
      r = @random_generator.rand(range.to_i + 1)
      smallest = [limit, limit2].min
      r + smallest
    end

    def choose(list)
      list.to_a.choose
    end

    def sleep(seconds)
      last = Thread.current.thread_variable_get :sonic_pi_spider_time
      now = Time.now

      new_t = last + seconds
      if now > new_t
        Thread.current.priority = 2
        __message "Can't keep up..."
      else
        Thread.current.priority = 1
        Kernel.sleep new_t - now
      end

      Thread.current.thread_variable_set :sonic_pi_spider_time, new_t

      ## reset control deltas now that time has advanced
      Thread.current.thread_variable_set :sonic_pi_control_deltas, {}
    end

    def sync(sync_id, val = nil)
      __no_kill_block do
        Kernel.sleep @sync_real_sleep_time
        @events.event("/spider_thread_sync/" + sync_id.to_s, {:time => Thread.current.thread_variable_get(:sonic_pi_spider_time), :val => val})
      end
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

    def in_thread(name=nil, &block)
      parent_t = Thread.current

      # Get copy of thread locals whilst we're sure they're not being modified
      # as we're in the thread parent_t
      parent_t_vars = {}
      parent_t.thread_variables.each do |v|
        parent_t_vars[v] = parent_t.thread_variable_get(v)
      end

      job_id = __current_job_id
      reg_with_parent_completed = Promise.new

      # Create the new thread
      t = Thread.new do

        # Copy thread locals across from parent thread to this new thread
        parent_t_vars.each do |k,v|
          Thread.current.thread_variable_set(k, v) unless k.to_s.start_with? "sonic_pi__not_inherited__"
        end

        # Reset subthreads thread local to the empty set. This shouldn't
        # be inherited from the parent thread.
        Thread.current.thread_variable_set :sonic_pi_spider_subthreads, Set.new

        # Give new thread a new subthread mutex
        Thread.current.thread_variable_set :sonic_pi_spider_subthread_mutex, Mutex.new

        # Give new thread a new no_kill mutex
        Thread.current.thread_variable_set :sonic_pi_spider_no_kill_mutex, Mutex.new

        # Wait for parent to deliver promise. Throws an exception if
        # parent dies before the promise is delivered, thus stopping
        # this thread from continually waiting for forgotten promises...
        wait_for_parent_thread!(parent_t, reg_with_parent_completed)

        # Attempt to associate the current thread with job with
        # job_id. This will kill the current thread if job is no longer
        # running.
        job_subthread_add(job_id, Thread.current, name)

        # Actually run the thread code specified by the user!
        begin
          block.call
        rescue Exception => e
          __error "Thread #{name} died: #{e.inspect}", e
        end

        # Disassociate thread with job as it has now finished
        job_subthread_rm(job_id, Thread.current)

        Thread.new do
          # wait for all subthreads to finish before removing self from
          # the subthread tree
          __join_subthreads(t)
          parent_t.thread_variable_get(:sonic_pi_spider_subthread_mutex).synchronize do
            parent_t.thread_variable_get(:sonic_pi_spider_subthreads).delete(Thread.current)
          end
        end
      end

      # Whilst we know that the new thread is waiting on the promise to
      # be delivered, we can now add it to our list of subthreads. Using
      # the promise means that we can be assured that killing this
      # current thread won't create a zombie child thread as the child
      # thread will only continue exiting after it has been sucessfully
      # registered.

      parent_t.thread_variable_get(:sonic_pi_spider_subthread_mutex).synchronize do
        subthreads = parent_t.thread_variable_get :sonic_pi_spider_subthreads
        subthreads.add(t)
      end

      # Allow the subthread to continue running
      reg_with_parent_completed.deliver! true

      # Return subthread
      t
    end
  end
end
