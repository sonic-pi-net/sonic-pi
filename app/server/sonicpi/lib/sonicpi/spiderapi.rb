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
require_relative "version"

module SonicPi
  module SpiderAPI

    include SonicPi::DocSystem

    def live_loop(name, &block)
      raise "live_loop must be called with a code block" unless block

      define(name, &block)

      in_thread(name: name) do
        loop do
          cue name
          send(name)
        end
      end
    end



    def after(times, &block)
      raise "after must be called with a code block" unless block

      times.each_with_index do |t, idx|
        in_thread do
          sleep t
          case block.arity
          when 0
            block.call
          when 1
            block.call(t)
          when 2
            block.call(t, idx)
          end
        end
      end
    end




    def version
      @version
    end
    doc name:           :version,
        introduced:     Version.new(2,0,0),
        summary:        "Get current version information",
        args:           [],
        opts:           nil,
        accepts_block: false,
        doc: "Return information representing the current version of Sonic Pi. This information may be further inspected with version.major, version.minor, version.patch and version.dev",
        examples: ["
puts version # => Prints out the current version such as 2,0,0",
"
puts version.major # => Prints out the major version number such as 2",
"
puts version.minor # => Prints out the minor version number such as 0",
"
puts version.patch # => Prints out the patch level for this version such as 0"]




    def defonce(name, *opts, &block)
      raise "defonce must be called with a code block" unless block
      args_h = resolve_synth_opts_hash_or_array(opts)
      if args_h[:override] || !(@user_methods.method_defined? name)
        val = block.yield
        val_block = lambda{val}
        define(name, &val_block)
        __info "Evaluating defonce #{name}"
      else
        __info "Not re-evaluating defonce #{name}"
      end
    end
    doc name:           :defonce,
        introduced:     Version.new(2,0,0),
        summary:        "Define a named value only once",
        args:           [[:name, :symbol]],
        opts:           {:override => false},
        accepts_block: true,
        doc:            "Allows you assign the result of some code to a name with the property that the code will only execute once therefore stopping re-definitions. This is useful for defining values that you use in your compositions but you don't want to reset every time you press run. You may force the block to execute again regardless of whether or not it has executed once already by using the override option (see examples).",
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
end",

"
defonce :bar do
  50
end

play bar # plays 50

defonce :bar do # This redefinition doesn't work due to the behaviour of defonce
  70
end

play bar # Still plays 50

defonce :bar, override: true do  # Force definition to take place with override option
  80
end

play bar # plays 80"]




    def define(name, &block)
      raise "define must be called with a code block" unless block
      if @user_methods.method_defined? name
        __info "Redefining #{name}"
      else
        __info "Defining #{name}"
      end
      @user_methods.send(:define_method, name, &block)
    end
    doc name:           :define,
        introduced:     Version.new(2,0,0),
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




    # def on_keypress(&block)
    #   @keypress_handlers[:foo] = block
    # end
    # doc name:           :on_keypress,
    #     summary:        "",
    #     args:           [],
    #     opts:           nil,
    #     accepts_block:  true,
    #     doc:            "",
    #     examples:       [],
    #     hide:           true




    def comment(*args, &block)
      #do nothing!
    end
    doc name:           :comment,
        introduced:     Version.new(2,0,0),
        summary:        "Block level commenting",
        args:           [],
        opts:           nil,
        accepts_block:  true,
        doc:            "Does not evaluate any of the code within the block. However, any optional args passed before the block *will* be evaluated although they will be ignored. See uncomment for switching commenting off without having to remove the comment form.",
        examples:       ["comment do # starting a block level comment:
  play 50 # not played
  sleep 1 # no sleep happens
  play 62 # not played
end"]




    def uncomment(*args, &block)
      block.call
    end
    doc name:           :uncomment,
        introduced:     Version.new(2,0,0),
        summary:        "Block level comment ingoring",
        args:           [],
        opts:           nil,
        accepts_block:  true,
        doc:            "Evaluates all of the code within the block. Use to reverse the effect of the comment without having to explicitly remove it.",
        examples:       ["uncomment do # starting a block level comment:
  play 50 # played
  sleep 1 # sleep happens
  play 62 # played
end"]




    def print(output)
     __delayed_user_message output
    end
    doc name:          :print,
        introduced:     Version.new(2,0,0),
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
        introduced:     Version.new(2,0,0),
        summary:       "Display a message in the output pane",
        args:           [[:output, :string]],
        opts:           nil,
        accepts_block:  false,
        doc:           "Displays the information you specify as a string inside the output pane. This can be a number, symbol, or a string itself. Useful for debugging. Synonym for print.",
        examples:      [
"print \"hello there\"   #=> will print the string \"hello there\" to the output pane",
"print 5               #=> will print the number 5 to the output pane",
"print foo             #=> will print the contents of foo to the output pane"]




    def dice(num_sides=6)
      rrand_i(1, num_sides)
    end
    doc name:           :dice,
        introduced:     Version.new(2,0,0),
        summary:        "Random dice throw",
        args:           [[:num_sides, :number]],
        opts:           nil,
        accepts_block:  false,
        doc:            "Throws a dice with the specified num_sides (defaults to 6) and returns the score as a number between 1 and num_sides.",
        examples:      [
"
dice # will return a number between 1 and 6 inclusively
     # (with an even probaility distribution).",
"
dice 3 # will return a number between 1 and 3 inclusively"]



    def one_in(num)
      rrand_i(1, num) == 1
    end
    doc name:           :one_in,
        introduced:     Version.new(2,0,0),
        summary:        "Random true value with specified probability",
        args:           [[:num, :number]],
        opts:           nil,
        accepts_block:  false,
        doc:            "Returns true or false with a specified probability - it will return true every one in num times where num is the param you specify",
        examples:      [
"
one_in 2 # will return true with a probablility of 1/2, false with probability 1/2",
"
one_in 3 # will return true with a probability of 1/3, false with a probability of 2/3",
"
one_in 100 # will return true with a probability of 1/100, false with a probability of 99/100"]




    def rrand(min, max)
      range = (min - max).abs
      rgen = Thread.current.thread_variable_get :sonic_pi_spider_random_generator
      r = rgen.rand(range.to_f)
      smallest = [min, max].min
      r + smallest
    end
    doc name:           :rrand,
        introduced:     Version.new(2,0,0),
        summary:        "Generate a random float between two numbers",
        args:           [[:min, :number], [:max, :number]],
        opts:           nil,
        accepts_block:  false,
        doc:            "Given two numbers, this produces a float between the supplied min and max values exclusively. Both min and max need to be supplied. For random integers, see rrand_i",
        examples:      [
"
print rrand(0, 10) #=> will print a number like 8.917730007820797 to the output pane",
"
loop do
  play rrand(60, 72) #=> Will play a random non-integer midi note between C4 (60) and C5 (72) such as 67.3453 or 71.2393
  sleep 0.125
end"]




    def rrand_i(min, max)
      range = (min - max).abs
      rgen = Thread.current.thread_variable_get :sonic_pi_spider_random_generator
      r = rgen.rand(range.to_i + 1)
      smallest = [min, max].min
      (r + smallest).to_f
    end
    doc name:           :rrand_i,
        introduced:     Version.new(2,0,0),
        summary:        "Generate a random whole number between two points inclusively",
        args:           [[:min, :number], [:max, :number]],
        opts:           nil,
        accepts_block: false,
        doc:            "Given two numbers, this produces a whole number between the min and max you supplied inclusively. Both min and max need to be supplied. For random floats, see rrand",
        examples:      [
"
print rrand_i(0, 10) #=> will print a random number between 0 and 10 (e.g. 4.0, 0.0 or 10.0) to the output pane",
"
loop do
  play rrand_i(60, 72) #=> Will play a random midi note between C4 (60) and C5 (72)
  sleep 0.125
end"]




    def rand(max=1)
      rgen = Thread.current.thread_variable_get :sonic_pi_spider_random_generator
      rgen.rand(max.to_f)
    end
    doc name:           :rand,
        introduced:     Version.new(2,0,0),
        summary:        "Generate a random float below a value",
        args:           [[:max, :number]],
        opts:           nil,
        accepts_block:  false,
        doc:            "Given a max number, produces a float between 0 and the supplied max value. With no args, returns a random value between 0 and 1.",
        examples:      [
"
print rand(0.5) #=> will print a number like 0.397730007820797 to the output pane"]




    def rand_i(max=2)
      rgen = Thread.current.thread_variable_get :sonic_pi_spider_random_generator
      rgen.rand(max.to_i).to_f
    end
    doc name:           :rand_i,
        introduced:     Version.new(2,0,0),
        summary:        "Generate a random whole number float below a value",
        args:           [[:max, :number]],
        opts:           nil,
        accepts_block:  false,
        doc:            "Given a max number, produces a whole numberfloat between 0 and the supplied max value. With no args returns either 0.0 or 1.0",
        examples:      [
"
print rand_i(10) #=> will print a number like 7.0 to the output pane"]




    def choose(list)
      list.to_a.choose
    end
    doc name:           :choose,
        introduced:     Version.new(2,0,0),
        summary:        "Random list selection",
        args:           [[:list, :array]],
        opts:           nil,
        accepts_block:  false,
        doc:            "Choose an element at random from a list (array).",
        examples:       [
"loop do
  play choose([60, 64, 67]) #=> plays one of 60, 64 or 67 at random
  sleep 1
  play chord(:c, :major).choose #=> You can also call .choose on the list
  sleep 1
end"]




    def use_random_seed(seed, &block)
      raise "use_random_seed does not work with a block. Perhaps you meant with_random_seed" if block
      Thread.current.thread_variable_set :sonic_pi_spider_random_generator, Random.new(seed)
    end
    doc name:          :use_random_seed,
        introduced:     Version.new(2,0,0),
        summmary: "",
        doc:            "Resets the random number generator to the specified seed. All subsequently generated random numbers will use this new generator and the current generator is discarded. Use this to change the sequence of random numbers in your piece in a way that can be reproduced",
        args:          [[:seed, :number]],
        opts:          nil,
        accepts_block: false,
        examples:      ["
use_random_seed 1 # reset random seed to 1
puts rand # => 0.417022004702574
use_random_seed 1 # reset random seed back to 1
puts rand  #=> 0.417022004702574
"]




    def with_random_seed(seed, &block)
      raise "with_random_seed requires a block. Perhaps you meant use_random_seed" unless block
      current_rgen = Thread.current.thread_variable_get :sonic_pi_spider_random_generator
      Thread.current.thread_variable_set :sonic_pi_spider_random_generator, Random.new(seed)
      block.call
      Thread.current.thread_variable_set :sonic_pi_spider_random_generator, current_rgen
    end
    doc name:          :with_random_seed,
        introduced:     Version.new(2,0,0),
        summary:       "Specify random seed for code block",
        doc:            "Resets the random number generator to the specified seed for the specified code block. All generated random numbers within the code block will use this new generator. Once the code block has completed, the original generator is restored and the code block generator is discarded.",
        args:          [[:seed, :number]],
        opts:          nil,
        accepts_block: true,
        examples:      ["
use_random_seed 1 # reset random seed to 1
puts rand # => 0.417022004702574
puts rand  #=> 0.7203244934421581
use_random_seed 1 # reset it back to 1
puts rand # => 0.417022004702574
with_random_seed 1 do # reset seed back to 1 just for this block
  puts rand # => 0.417022004702574
  puts rand #=> 0.7203244934421581
end
puts rand # => 0.7203244934421581
          # notice how the original generator is restored"]




    def use_bpm(bpm, &block)
      raise "use_bpm does not work with a block. Perhaps you meant with_bpm" if block
      sleep_mul = 60.0 / bpm
      Thread.current.thread_variable_set(:sonic_pi_spider_sleep_mul, sleep_mul)
    end
    doc name:           :use_bpm,
        introduced:     Version.new(2,0,0),
        summary:        "Set the tempo",
        doc:            "Sets the tempo in bpm (beats per minute) for everything afterwards. Affects all subsequent calls to sleep and all temporal synth arguments which will be scaled to match the new bpm. If you wish to bypass scaling in calls to sleep, see the fn rt. Also, if you wish to bypass time scaling in synth args see use_arg_bpm_scaling. See also with_bpm for a block scoped version of use_bpm.",
        args:           [[:bpm, :number]],
        opts:           nil,
        accepts_block:  false,
        examples:       [
"# default tempo is 60 bpm
4.times do
  play 50, attack: 0.5, release: 0.25 # attack is 0.5s and release is 0.25s
  sleep 1 # sleep for 1 second
end

sleep 2  # sleep for 2 seconds

# Let's make it go faster...
use_bpm 120  # double the bpm
4.times do
  play 62, attack: 0.5, release: 0.25 # attack is scaled to 0.25s and release is now 0.125s
  sleep 1 # actually sleeps for 0.5 seconds
end

sleep 2 # sleep for 1 second

# Let's make it go even faster...
use_bpm 240  #  bpm is 4x original speed!
8.times do
  play 62, attack: 0.5, release: 0.125 # attack is scaled to 0.25s and release is now 0.0625s
  sleep 1 # actually sleeps for 0.25 seconds
end

"]




    def with_bpm(bpm, &block)
      raise "with_bpm must be called with a block. Perhaps you meant use_bpm" unless block
      current_mul = Thread.current.thread_variable_get(:sonic_pi_spider_sleep_mul)
      sleep_mul = 60.0 / bpm
      Thread.current.thread_variable_set(:sonic_pi_spider_sleep_mul, sleep_mul)
      block.call
      Thread.current.thread_variable_set(:sonic_pi_spider_sleep_mul, current_mul)
    end
    doc name:           :with_bpm,
        introduced:     Version.new(2,0,0),
        summary:        "Set the tempo for the code block",
        doc:            "Sets the tempo in bpm (beats per minute) for everything in the given block. Affects all containing calls to sleep and all temporal synth arguments which will be scaled to match the new bpm. See also use_bpm",
        args:           [[:bpm, :number]],
        opts:           nil,
        accepts_block:  true,
        examples:       [
"# default tempo is 60 bpm
4.times do
  sample :drum_bass_hard
  sleep 1 # sleeps for 1 second
end

sleep 5 # sleeps for 5 seconds

# with_bpm sets a tempo for everything between do ... end (a block)
# Hear how it gets faster?
with_bpm 120 do  # set bpm to be twice as fast
  4.times do
    sample :drum_bass_hard
    sleep 1 # now sleeps for 0.5 seconds
  end
end

sleep 5

# bpm goes back to normal
4.times do
  sample :drum_bass_hard
  sleep 1 # sleeps for 1 second
end"]




    def current_bpm
      60.0 / Thread.current.thread_variable_get(:sonic_pi_spider_sleep_mul)
    end
    doc name:          :current_bpm,
        introduced:    Version.new(2,0,0),
        summary:       "Get current tempo",
        doc:           "Returns the current tempo as a bpm value.",
        args:          [],
        opts:          nil,
        accepts_block: false,
        examples:      ["
puts current_bpm # Print out the current bpm"]




    def rt(t)
      t / Thread.current.thread_variable_get(:sonic_pi_spider_sleep_mul)
    end
    doc name:          :rt,
        introduced:    Version.new(2,0,0),
        summary:       "Real time conversion",
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
      return if seconds == 0
      # Grab the current virtual time
      last_vt = Thread.current.thread_variable_get :sonic_pi_spider_time

      # Schedule messages
      __schedule_delayed_blocks_and_messages!

      # Now get on with syncing the rest of the sleep time...

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
        introduced:     Version.new(2,0,0),
        summary:        "Wait for duration",
        doc:            "Wait for a number of seconds before triggering the next command. Seconds are scaled to the current bpm setting.",
        args:           [[:seconds, :number]],
        opts:           nil,
        accepts_block:  false,
        examples:       [
"# Without calls to sleep, all sounds would happen at once:

play 50  # This is actually a chord with all notes played simultaneously
play 55
play 62

sleep 1  # Create a gap, to allow a moment's pause for reflection...

play 50  # Let's try the chord again, but this time with sleeps:
sleep 0.5 # With the sleeps, we turn a chord into an arpegio
play 55
sleep 0.5
play 62",

"
# The amount of time sleep pauses for is scaled to match the current bpm. The default bpm is 60. Let's double it:

use_bpm 120
play 50
sleep 1 # This actually sleeps for 0.5 seconds as we're now at double speed
play 55
sleep 1
play 62

# Let's go down to half speed:

use_bpm 30
play 50
sleep 1 # This now sleeps for 2 seconds as we're now at half speed.
play 55
sleep 1
play 62
"]


    def wait(time)
      if time.is_a? Symbol
        sync(time)
      else
        sleep(time)
      end
    end
    doc name:           :wait,
        introduced:     Version.new(2,0,0),
        summary:        "Wait for duration",
        doc:            "Synonym for sleep - see sleep",
        args:           [[:seconds, :number]],
        opts:           nil,
        accepts_block:  false,
        examples:       []




    def cue(cue_id)
      __no_kill_block do
        Kernel.sleep @sync_real_sleep_time
payload = {
          :time => Thread.current.thread_variable_get(:sonic_pi_spider_time),
          :run => current_job_id
         }
        __delayed_highlight_message "cue #{cue_id.to_sym.inspect}"
        @events.event("/spider_thread_sync/" + cue_id.to_s, payload)
      end
    end
    doc name:           :cue,
        introduced:     Version.new(2,0,0),
        summary:        "Cue other threads",
        doc:            "Send a heartbeat synchronisation message containing the (virtual) timestamp of the current thread. Useful for syncing up external threads via the sync fn.",
        args:           [[:cue_id, :symbol]],
        opts:           {:message => nil},
        accepts_block:  false,
        examples:       ["
in_thread do
  sync :foo # this parks the current thread waiting for a foo cue message to be received.
  sample :ambi_lunar_land
end

sleep 5

cue :foo # We send a cue message from the main thread.
          # This then unblocks the thread above and we then hear the sample",

"
in_thread do   # Start a metronome thread
  loop do      # Loop forever:
    cue :tick # sending tick heartbeat messages
    sleep 0.5  # and sleeping for 0.5 seconds between ticks
  end
end

# We can now play sounds using the metronome.
loop do                    # In the main thread, just loop
  sync :tick               # waiting for :tick cue messages
  sample :drum_heavy_kick  # after which play the drum kick sample
end",

"
in_thread do   # Start a metronome thread
  loop do      # Loop forever:
    cue [:foo, :bar, :baz].choose # sending one of three tick heartbeat messages randomly
    sleep 0.5  # and sleeping for 0.5 seconds between ticks
  end
end

# We can now play sounds using the metronome:

in_thread do
  loop do                    # In the main thread, just loop
    sync :foo               # waiting for :foo cue messages
    sample :elec_beep  # after which play the elec beep sample
  end
end

in_thread do
  loop do                    # In the main thread, just loop
    sync :bar               # waiting for :bar cue messages
    sample :elec_flip  # after which play the elec flip sample
  end
end

in_thread do
  loop do                    # In the main thread, just loop
    sync :baz               # waiting for :baz cue messages
    sample :elec_blup  # after which play the elec blup sample
  end
end"
]




    def sync(cue_id)
      __delayed_highlight3_message "sync #{cue_id.to_sym.inspect}"
      __schedule_delayed_blocks_and_messages!
      p = Promise.new
      @events.oneshot_handler("/spider_thread_sync/" + cue_id.to_s) do |payload|
        p.deliver! payload
      end
      payload = p.get
      time = payload[:time]
      run_id = payload[:run]
      Thread.current.thread_variable_set :sonic_pi_spider_time, time
      __delayed_highlight2_message "synced #{cue_id.to_sym.inspect} (Run #{run_id})"
      cue_id
    end
    doc name:           :sync,
        introduced:     Version.new(2,0,0),
        summary:        "Sync with other threads",
        doc:            "Pause/block the current thread until a cue heartbeat with a matching cue_id is received. When a matching cue message is received, unblock the current thread, and continue execution with the virtual time set to match the thread that sent the cue heartbeat. The current thread is therefore synced to the cue thread.",
        args:           [[:cue_id, :symbol]],
        opts:           nil,
        accepts_block:  false,
        examples:       ["
in_thread do
  sync :foo # this parks the current thread waiting for a foo sync message to be received.
  sample :ambi_lunar_land
end

sleep 5

cue :foo # We send a sync message from the main thread.
          # This then unblocks the thread above and we then hear the sample",

"
in_thread do   # Start a metronome thread
  loop do      # Loop forever:
    cue :tick # sending tick heartbeat messages
    sleep 0.5  # and sleeping for 0.5 seconds between ticks
  end
end

# We can now play sounds using the metronome.
loop do                    # In the main thread, just loop
  sync :tick               # waiting for :tick sync messages
  sample :drum_heavy_kick  # after which play the drum kick sample
end",

"
in_thread do   # Start a metronome thread
  loop do      # Loop forever:
    cue [:foo, :bar, :baz].choose # sending one of three tick heartbeat messages randomly
    sleep 0.5  # and sleeping for 0.5 seconds between ticks
  end
end

# We can now play sounds using the metronome:

in_thread do
  loop do                    # In the main thread, just loop
    sync :foo               # waiting for :foo sync messages
    sample :elec_beep  # after which play the elec beep sample
  end
end

in_thread do
  loop do                    # In the main thread, just loop
    sync :bar               # waiting for :bar sync messages
    sample :elec_flip  # after which play the elec flip sample
  end
end

in_thread do
  loop do                    # In the main thread, just loop
    sync :baz               # waiting for :baz sync messages
    sample :elec_blup  # after which play the elec blup sample
  end
end"]




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
      rgen = Thread.current.thread_variable_get :sonic_pi_spider_random_generator
      new_rand_seed = rgen.rand(999999999999999999999999999999999999999)

      # Create the new thread
      t = Thread.new do
        Thread.current.thread_variable_set(:sonic_pi_thread_group, :job_subthread)

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
        Thread.current.thread_variable_set :sonic_pi_spider_random_generator, Random.new(new_rand_seed)

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
        introduced:     Version.new(2,0,0),
        summary:        "Run code block at the same time",
        doc:            "Execute a given block (between do ... end) in a new thread. Use for playing multiple 'parts' at once. Each new thread created inherits all the use/with defaults of the parent thread such as the time, current synth, bpm, default synth args, etc. Despite inheriting defaults from the parent thread, any modifications of the defaults in the new thread will *not* affect the parent thread. Threads may be named with the name: optional arg. Named threads will print their name in the logging pane when they print their activity. Finally, if you attempt to create a new named thread with a name that is already in use by another executing thread, no new thread will be created.",
        args:           [],
        opts:           {:name => nil},
        accepts_block:  true,
        examples:       [
"
loop do      # If you write two loops one after another like this,
  play 50    # then only the first loop will execute as the loop acts
  sleep 1    # like a trap not letting the flow of control out
end

loop do      # This code is never executed.
  play 55
  sleep 0.5
end ",

"

# In order to play two loops at the same time, the first loops need to be in a thread:

# By wrapping out loop in an in_thread block, we split the
# control flow into two parts. One flows into the loop (a) and
# the other part flows immediately after the in_thread block (b).
# both parts of the control flow execute at exactly the same time.

in_thread do
  # (a)
  loop do
    # (a)
    play 50
    sleep 1
  end
end

# (b)

loop do      # This loop is executed thanks to the thread above
  play 55
  sleep 0.5
end",

"
use_bpm 120  # Set the bpm to be double rate
use_synth :dsaw  # Set the current synth to be :dsaw

in_thread do     # Create a new thread
  play 50        # Play note 50 at time 0
  use_synth :fm  # Switch to fm synth (only affects this thread)
  sleep 1        # sleep for 0.5 seconds (as we're double rate)
  play 38        # Play note 50 at time 0.5
end

play 62          # Play note 62 at time 0 (with dsaw synth)
sleep 2          # sleep 1s
play 67          # Play note 67 at time 1s (also with dsaw synth)
",

"
in_thread(name: :foo) do # Here we've created a named thread
  loop do
    sample :drum_bass_hard
    sleep 1
  end
end

in_thread(name: :foo) do # This thread isn't created as the name is
  loop do                # the same as the previous thread which is
    sample :elec_chime   # still executing.
    sleep 0.5
  end
end",

"
 # Named threads work well with functions for live coding:
define :foo do  # Create a function foo
  play 50       # which does something simple
  sleep 1       # and sleeps for some time
end

in_thread(name: :main) do  # Create a named thread
  loop do                  # which loops forever
    foo                    # calling our function
  end
end

# We our now free to modify the contents of :foo and re-run the entire buffer.
# We'll hear the affect immediately without having to stop and re-start the code.
# This is because our fn has been redefined, (which our thread will pick up) and
# due to the thread being named, the second re-run will not create a new similarly
# named thread. This is a nice pattern for live coding.
"]
  end
end
