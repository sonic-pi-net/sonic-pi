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


    def comment(*args, &block)
      #do nothing!
    end
    doc name:           :comment,
        summary:        "Block level commenting",
        args:           [],
        opts:           nil,
        accepts_block:  true,
        doc:            "Does not evaluate any of the code within the block. However, any args passed before the block *will* be evaluated.",
        examples:       ["comment do # starting a block level comment:
  play 50 # not played
  sleep 1 # no sleep happens
  play 62 @ not played
end"]


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
        summary:        "Generate a random float between two numbers",
        args:           [[:min, :number], [:max, :number]],
        opts:           nil,
        accepts_block:  false,
        doc:            "Given two numbers, this produces a float between the min and max you supplied. It needs an argument for both min and max in order to work. For random integers, see rrand_i",
        examples:      [
"print rrand(0, 10) #=> will print a number like 8.917730007820797 to the output pane",
"print rrand(0, 10).to_i #=> See also rrand_i(0, 10). Using Ruby's to_i method you can make sure it produces random integers. This will give you numbers like 8 instead of 8.917730007820797",
"play rrand(60, 72).to_i #=> Will play a random midi note between C4 (60) and C5 (72)",
"# To assign a random number to a variable (sometimes good to make your code more readable) you can use an advanced feature of Ruby called a lambda.
# You just need to add .call to the variable when you want to use it
my_random_note = -> { rrand(60, 72).to_i }
loop do
  play my_random_note.call
  sleep 1
end"]




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
        doc:            "Given two numbers, this produces a float between the min and max you supplied. It needs an argument for both min and max in order to work. For random floats, see rrand",
        examples:      [
"print rrand_i(0, 10) #=> will print a random number between 0 and 10 (e.g. 4) to the output pane",
"play rrand_i(60, 72) #=> Will play a random midi note between C4 (60) and C5 (72)",
"# To assign a random number to a variable (sometimes good to make your code more readable) you can use an advanced feature of Ruby called a lambda.
# You just need to add .call to the variable when you want to use it
my_random_note = -> { rrand_i(60, 72) }
loop do
  play my_random_note.call
  sleep 1
end"]




    def choose(list)
      list.to_a.choose
    end
    doc name:           :choose,
        args:           [[:list, :array]],
        opts:           nil,
        accepts_block:  false,
        doc:            "Choose an element at random from a list (array).",
        examples:       [
"loop do
  play [60, 64, 67].choose #=> plays one of 60, 64 or 67 at random
  sleep 1
  play chord(:c, :major).choose #=> works on chords and scales too
  sleep 1
end"]




    def use_bpm(bpm, &block)
      raise "use_bpm does not work with a block. Perhaps you meant with_bpm" if block
      sleep_mul = 60.0 / bpm
      Thread.current.thread_variable_set(:sonic_pi_spider_sleep_mul, sleep_mul)
    end
    doc name:           :use_bpm,
        doc:            "Sets the tempo in bpm (beats per minute) for everything afterwards. See also with_bpm",
        args:           [[:bpm, :number]],
        opts:           nil,
        accepts_block:  false,
        examples:       [
"# default tempo is 60 bpm
4.times do
  sample :drum_bass_hard
  sleep 1
end

sleep 5

# use_bpm will affect everything following it
# Hear how it gets faster?
use_bpm 140
4.times do
  sample :drum_bass_hard
  sleep 1
end"]




    def with_bpm(bpm, &block)
      raise "with_bpm must be called with a block. Perhaps you meant use_bpm" unless block
      current_mul = Thread.current.thread_variable_get(:sonic_pi_spider_sleep_mul)
      sleep_mul = 60.0 / bpm
      Thread.current.thread_variable_set(:sonic_pi_spider_sleep_mul, sleep_mul)
      block.call
      Thread.current.thread_variable_set(:sonic_pi_spider_sleep_mul, current_mul)
    end
    doc name:           :with_bpm,
        doc:            "Sets the tempo in bpm (beats per minute) for everything in a given block. See also use_bpm",
        args:           [[:bpm, :number]],
        opts:           nil,
        accepts_block:  true,
        examples:       [
"# default tempo is 60 bpm
4.times do
  sample :drum_bass_hard
  sleep 1
end

sleep 5

# with_bpm sets a tempo for everything between do ... end (a block)
# Hear how it gets faster?
with_bpm 140 do
  4.times do
    sample :drum_bass_hard
    sleep 1
  end
end

sleep 5

# bpm goes back to normal
4.times do
  sample :drum_bass_hard
  sleep 1
end"]




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




    def rt(t)
      t / Thread.current.thread_variable_get(:sonic_pi_spider_sleep_mul)
    end
    doc name:          :rt,
        doc:           "Real time representation. Returns the amount of beats for the value in real-time seconds. Useful for bypassing any bpm scaling",
        args:          [[:seconds, :number]],
        opts:          nil,
        accepts_block: false,
        examples:      ["
use_bpm 120  # modifies all time to be half
play 50
sleep 1      # actually sleeps for half of a second
play 62
sleep rt(1)  # bypasses bpm scaling and sleeps for a second
play 72"]


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
        doc:            "Wait for a number of seconds before triggering the next command. Seconds are scaled to the current bpm setting.",
        args:           [[:seconds, :number]],
        opts:           nil,
        accepts_block:  false,
        examples:       [
"# If SonicPi didn't have sleep all the sounds would happen at once!
# Every other command (play, sample etc.) will try to run as fast as possible and move on to the next command.
# Because we're making music, we need to tell SonicPi to wait for a sound or a note to finish before moving on.
# We do this by using sleep with an argument for how many seconds to wait
play 60
sleep 1
play 60
sleep 1
play 67
sleep 1
play 67
sleep 1
play 69
sleep 0.5
play 71
sleep 0.5
play 72
sleep 0.5
play 69
sleep 0.5
play 67
sleep 2",
"# When we change the current tempo with the use_bpm command the 'seconds' are changed to fit the music to the new tempo
use_bpm 130
play 60
sleep 1
play 60
sleep 1
play 67
sleep 1
play 67
sleep 1
play 69
sleep 0.5
play 71
sleep 0.5
play 72
sleep 0.5
play 69
sleep 0.5
play 67
sleep 2"]




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
        doc:            "Execute a given block (between do ... end) in a new thread. Use for playing multiple 'parts' at once.",
        args:           [],
        opts:           {:name => nil},
        accepts_block:  true,
        examples:       [
"# In thread is very useful in SonicPi for letting multiple parts play without inteferring with each other
# Any sleep or global settings like use_bpm can safely be used inside a thread without affecting the rest of the code.
# That means you can setup different threads for, say, drums and melody
in_thread(name: :drums) do # the (name: ...) here is optional
  use_bpm 120
  loop do
    sample :drum_bass_hard
    sleep 1
  end
end

# The loop above only happens inside the thread, which means the code can carry on executing
in_thread(name: :tune) do
  use_bpm 240
  loop do
    play chord(:a, :minor).choose
    sleep 1
  end
end"]
  end
end
