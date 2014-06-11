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
require_relative 'docsystem'

module SonicPi
  module SpiderAPI

    include SonicPi::DocSystem

    def defonce(name, override=false, &block)
      raise "defonce must be called with a code block" unless block
      if override || !(@user_methods.method_defined? name)
        val = block.yield
        val_block = lambda{val}
        @user_methods.send(:define_method, name, &val_block)
      else
        __info "Not re-defining #{name}"
      end
    end
    doc name:           :defonce,
        summary:        "Define a named value only once",
        args:           [[:name, :symbol]],
        opts:          nil,
        accepts_block: true,
        doc:            "Allows you assign the result of some code to a name with the property that the code will only execute once therefore stoping re-definitions. This is useful for defining values that you use in your compositions but you don't want to reset every time you press run.",
        examples:       ["

defonce :foo do  # Define a new function called foo
  sleep 1        # Sleep for a second in the function definition
  puts \"hello\" # Print hello
  10             # Return a value of 10
end

# Call foo on its own
puts foo # The run sleeps for a second and prints \"hello\" before returning 10

# Try it again:
puts foo # This time the run doesn't sleep or print anything out. However,  10 is still returned.



defonce :foo do # Try redefining foo
  puts \"you can't redefine me\"
  15
end

puts foo # We still don't see any printing or sleeping, and the result is still 10

# You can use foo anywhere you would use normal code.
# For example, in a block:
3.times do
  play foo  # play 10
end",]

    def define(name, &block)
      raise "define must be called with a code block" unless block
      @user_methods.send(:define_method, name, &block)
    end
    doc name:           :define,
        summary:        "Define a new function",
        args:           [[:name, :symbol]],
        opts:          nil,
        accepts_block: true,
        doc:            "Allows you to group a bunch of code and give it your own name for future re-use. Functions are very useful for structuring your code. They are also the gateway into live coding as you may redefine a function whilst a thread is calling it, and the next time the thread calls your function, it will use the latest definition.",
        examples:       ["
# Define a new function called foo
define :foo do
  play 50
  sleep 1
end

# Call foo on its own
foo

# You can use foo anywhere you would use normal code.
# For example, in a block:
3.times do
  foo
end",]



    def on_keypress(&block)
      @keypress_handlers[:foo] = block
    end
    doc name:           :on_keypress,
        summary:        "",
        args:           [],
        opts:           nil,
        accepts_block:  true,
        doc:            "",
        examples:       [],
        hide:           true




    def print(output)
     __delayed_user_message output
    end
    doc name:          :print,
        summary:       "Display a message in the output pane",
        args:          [[:output, :string]],
        opts:          nil,
        accepts_block: false,
        doc:           "Displays the information you specify as a string inside the output pane. This can be a number, symbol, or a string itself. Useful for debugging. Synonym for puts.",
        examples:      [
"print \"hello there\"   #=> will print the string \"hello there\" to the output pane",
"print 5               #=> will print the number 5 to the output pane",
"print foo             #=> will print the contents of foo to the output pane"]




    def puts(output)
      __delayed_user_message output
    end
    doc name:           :puts,
        summary:       "Display a message in the output pane",
        args:           [[:output, :string]],
        opts:           nil,
        accepts_block:  false,
        doc:           "Displays the information you specify as a string inside the output pane. This can be a number, symbol, or a string itself. Useful for debugging. Synonym for print.",
        examples:      [
"print \"hello there\"   #=> will print the string \"hello there\" to the output pane",
"print 5               #=> will print the number 5 to the output pane",
"print foo             #=> will print the contents of foo to the output pane"]




    def rrand(min, max)
      range = (min - max).abs
      r = @random_generator.rand(range.to_f)
      smallest = [min, max].min
      r + smallest
    end
    doc name:           :rrand,
        summary:        "",
        args:           [[:min, :number], [:max, :number]],
        opts:           nil,
        accepts_block:  false,
        doc:            "",
        examples:      []




    def rrand_i(min, max)
      range = (min - max).abs
      r = @random_generator.rand(range.to_i + 1)
      smallest = [min, max].min
      r + smallest
    end
    doc name:           :rrand_i,
        args:           [[:min, :number], [:max, :number]],
        opts:           nil,
        accepts_block: false,
        doc:            "",
        examples:       []




    def choose(list)
      list.to_a.choose
    end
    doc name:           :choose,
        args:           [[:list, :array]],
        opts:           nil,
        accepts_block:  false,
        doc:            "",
        examples:       []




    def use_bpm(bpm, &block)
      raise "use_bpm does not work with a block. Perhaps you meant with_bpm" if block
      sleep_mul = 60.0 / bpm
      Thread.current.thread_variable_set(:sonic_pi_spider_sleep_mul, sleep_mul)
    end
    doc name:           :use_bpm,
        doc:            "",
        args:           [[:bpm, :number]],
        opts:           nil,
        accepts_block:  false,
        examples:       []




    def with_bpm(bpm, &block)
      raise "with_bpm must be called with a block. Perhaps you meant use_bpm" unless block
      current_mul = Thread.current.thread_variable_get(:sonic_pi_spider_sleep_mul)
      sleep_mul = 60.0 / bpm
      Thread.current.thread_variable_set(:sonic_pi_spider_sleep_mul, sleep_mul)
      block.call
      Thread.current.thread_variable_set(:sonic_pi_spider_sleep_mul, current_mul)
    end
    doc name:           :with_bpm,
        doc:            "",
        args:           [],
        opts:           nil,
        accepts_block:  true,
        examples:       []

    def current_bpm
      60.0 / Thread.current.thread_variable_get(:sonic_pi_spider_sleep_mul)
    end
    doc name:          :current_bpm,
        doc:           "Returns the current bpm value.",
        args:          [],
        opts:          nil,
        accepts_block: false,
        examples:      ["
puts current_bpm # Print out the current bpm"]




    def sleep(seconds)
      # Grab the current virtual time
      last_vt = Thread.current.thread_variable_get :sonic_pi_spider_time
      __schedule_delayed_blocks_and_messages!
      # now get on with syncing the rest of the sleep time
      # Calculate the amount of time to sleep (take into account current bpm setting)
      sleep_time = seconds * Thread.current.thread_variable_get(:sonic_pi_spider_sleep_mul)
      # Calculate the new virtual time
      new_vt = last_vt + sleep_time

      # TODO: remove this, api shouldn't need to know about sound module
      sat = @mod_sound_studio.sched_ahead_time
      now = Time.now
      if now - (sat + 0.5) > new_vt
        raise "Timing Exception: thread got too far behind time"
      elsif (now - sat) > new_vt
        # Hard warning, system is too far behind, expect timing issues.
        Thread.current.priority = 20
        __delayed_serious_warning "Timing error: can't keep up..."
      elsif now > new_vt
        # Soft warning, system should work correctly, but is currently behind
        Thread.current.priority = 20
        __delayed_warning "Timing warning: running slightly behind..."
      else
        Kernel.sleep new_vt - now
      end

      Thread.current.thread_variable_set :sonic_pi_spider_time, new_vt
      ## reset control deltas now that time has advanced
      Thread.current.thread_variable_set :sonic_pi_control_deltas, {}
    end
    doc name:           :sleep,
        doc:            "",
        args:           [[:seconds, :number]],
        opts:           nil,
        accepts_block:  false,
        examples:       []




    def sync(sync_id, *opts)
      args_h = resolve_synth_opts_hash_or_array(opts)
      __no_kill_block do
        Kernel.sleep @sync_real_sleep_time
        @events.event("/spider_thread_sync/" + sync_id.to_s, {:time => Thread.current.thread_variable_get(:sonic_pi_spider_time), :val => args_h[:message]})
      end
    end
    doc name:           :sync,
        doc:            "",
        args:           [[:sync_id, :symbol]],
        opts:           {:message => nil},
        accepts_block:  false,
        examples:       []




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
    doc name:           :wait,
        doc:            "",
        args:           [[:sync_id, :symbol]],
        opts:           nil,
        accepts_block:  false,
        examples:       []




    def in_thread(*opts, &block)
      args_h = resolve_synth_opts_hash_or_array(opts)
      name = args_h[:name]

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
        main_t = Thread.current
        main_t.priority = 10

        Thread.new do
          Thread.current.thread_variable_set(:sonic_pi_thread_group, :in_thread_join)
          Thread.current.priority = -10
          # wait for all subthreads to finish before removing self from
          # the subthread tree
          main_t.join
          __join_subthreads(main_t)
          parent_t.thread_variable_get(:sonic_pi_spider_subthread_mutex).synchronize do
            parent_t.thread_variable_get(:sonic_pi_spider_subthreads).delete(main_t)
          end
        end

        Thread.current.thread_variable_set(:sonic_pi_thread_group, :job_subthread)
        # Copy thread locals across from parent thread to this new thread
        parent_t_vars.each do |k,v|
          Thread.current.thread_variable_set(k, v) unless k.to_s.start_with? "sonic_pi__not_inherited__"
        end


        Thread.current.thread_variable_set :sonic_pi_spider_users_thread_name, name if name

        Thread.current.thread_variable_set :sonic_pi_spider_delayed_blocks, []
        Thread.current.thread_variable_set :sonic_pi_spider_delayed_messages, []
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
          # ensure delayed jobs and messages are honoured for this
          # thread:
          __schedule_delayed_blocks_and_messages!
        rescue Exception => e
          if name
            __error "Thread #{name} died: #{e.inspect}", e
          else
            __error "Thread died: #{e.inspect}", e
          end
        end

        # Disassociate thread with job as it has now finished
        job_subthread_rm(job_id, Thread.current)
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
    doc name:           :in_thread,
        doc:            "",
        args:           [],
        opts:           {:name => nil},
        accepts_block:  true,
        examples:       []
  end
end
