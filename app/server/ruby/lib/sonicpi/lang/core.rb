## encoding: utf-8
#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++
require_relative 'support/docsystem'
require_relative "../version"
require_relative "../util"
require_relative "../runtime"

require 'active_support/inflector'

## TODO: create _* equivalents of all fns - for silent (i.e computation) versions

module SonicPi
  module Lang

    module Core

      include SonicPi::Lang::Support::DocSystem
      include SonicPi::Util

      class SonicPiError < StandardError ; end
      class AssertionError < SonicPiError ; end
      class TimingError < SonicPiError ; end
      class ZeroTimeLoopError < TimingError ; end
      class NotImmutableError < SonicPiError ; end
      class TimeTravelError < SonicPiError ; end
      class LiveLockError < SonicPiError ; end
      class DeprecationError < SonicPiError ; end
      class MapArgError < SonicPiError ; end

      THREAD_RAND_SEED_MAX = 10e20

      class TimeStateLookup
        def initialize(blk)
          @blk = blk
        end

        def [](*args)
          @blk.call(*args)
        end
      end

      def __cue_path_segment(s)
        s = String.new(s.to_s)
        s.gsub!(/[ \s#*,?\/\[\]{}]/, '_')
        s.freeze
      end

      def __cue_path(s, prefix='cue')
        s = s.to_s

        if s.start_with?('/')
          s = String.new("#{s}")
        else
          s = String.new("/#{prefix}/#{s}")
        end

        # convert all characters not allowed in
        # OSC path seg to underscores
        s.gsub!(/[ \s#*,?\[\]{}]/, '_')
        s.freeze
        s
      end

      def __sync_path(s)
        if s.is_a?(Symbol)
          return "/{cue,set,live_loop}/#{__cue_path_segment(s)}".freeze
        end

        s = s.to_s

        if s.start_with?('/')
          s = String.new("#{s}")
        else
          s = String.new("/cue/#{s}")
        end
        s.freeze
        s
      end

      def live_state(*args)
        get(*args)
      end

      def __cueset(k, val, prefix)
        if k.is_a?(Symbol)
          path = __cue_path(k, prefix)
          cue_path = [path, k.inspect]
        else
          cue_path = __cue_path(k)
          path = cue_path
        end

        t = __system_thread_locals.get(:sonic_pi_spider_time)
        b = __system_thread_locals.get(:sonic_pi_spider_beat)
        i = __current_thread_id
        d = __system_thread_locals.get(:sonic_pi_spider_thread_delta)
        __system_thread_locals.set_local(:sonic_pi_spider_thread_delta, d + 1)
        p = __system_thread_locals.get(:sonic_pi_spider_thread_priority, 0)
        m = current_bpm

        ce = CueEvent.new(t, p, i, d, b, m, cue_path, val)

        # update thread local time state cache
        cache = __system_thread_locals.get(:sonic_pi_spider_time_state_cache, [])
        cache.unshift [path, ce]
        __system_thread_locals.set_local(:sonic_pi_spider_time_state_cache, cache)



        # TODO - correctly add sched ahead time here after thread
        # waiting has been implemented
        # @register_cue_event_lambda.call(t, p, i, d, b, m, cue_path, val, current_sched_ahead_time)

        @register_cue_event_lambda.call(t, p, i, d, b, m, cue_path, val, __current_sched_ahead_time)

        unless __thread_locals.get(:sonic_pi_suppress_cue_logging)
          unless val
            __delayed_highlight_message "#{prefix} #{k.inspect}"
          else
            if is_list_like?(val)
              __delayed_highlight_message "#{prefix} #{k.inspect}, #{val}"
            else
              __delayed_highlight_message "#{prefix} #{k.inspect}, #{val.sp_log_inspect}"

            end
          end
        end

        val
      end

      def set(k, val)
        __cueset(k, val, "set")
      end

      def cue(k, *opts)
        splat_map_or_arr = []

        if opts.size == 1 && opts[0].is_a?(Hash)

          opts[0].each do |k, v|
            raise ArgumentError, "Invalid cue key type. Must be a Symbol" unless k.is_a? Symbol
            raise ArgumentError, "Invalid cue argument #{v.inspect} with key #{k.inspect} due to unrecognised type: (#{v.class}). Must be immutable -  currently accepted types: numbers, symbols, booleans, nil and frozen strings, or vectors/rings/frozen arrays/maps of immutable values" unless v.sp_thread_safe?
          end
          splat_map_or_arr = opts[0]
        else
          opts.each_with_index do |v, idx|

            v = v.__sp_make_thread_safe
            raise ArgumentError, "Invalid cue argument #{v.inspect} in position #{idx} due to unrecognised type: (#{v.class}). Must be immutable -  currently accepted types: numbers, symbols, booleans, nil and frozen strings, or vectors/rings/frozen arrays/maps of immutable values" unless v.sp_thread_safe?
          end
          splat_map_or_arr = opts.freeze
        end

        __cueset(k, splat_map_or_arr, "cue")
      end
      doc name:           :cue,
          introduced:     Version.new(2,0,0),
          summary:        "Cue other threads",
          doc:            "Send a heartbeat synchronisation message containing the (virtual) timestamp of the current thread. Useful for syncing up external threads via the `sync` fn. Any opts which are passed are given to the thread which syncs on the `cue_id`. The values of the opts must be immutable. Currently numbers, symbols, booleans, nil and frozen strings, or vectors/rings/frozen arrays/maps of immutable values are supported.",
          args:           [[:cue_id, :symbol]],
          opts:           {:your_key    => "Your value",
                           :another_key => "Another value",
                           :key         => "All these opts are passed through to the thread which syncs"},
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
      cue :tick  # sending tick heartbeat messages
      sleep 0.5  # and sleeping for 0.5 beats between ticks
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
      sleep 0.5  # and sleeping for 0.5 beats between ticks
    end
  end

  # We can now play sounds using the metronome:

  in_thread do
    loop do              # In the main thread, just loop
      sync :foo          # waiting for :foo cue messages
      sample :elec_beep  # after which play the elec beep sample
    end
  end

  in_thread do
    loop do              # In the main thread, just loop
      sync :bar          # waiting for :bar cue messages
      sample :elec_flip  # after which play the elec flip sample
    end
  end

  in_thread do
    loop do              # In the main thread, just loop
      sync :baz          # waiting for :baz cue messages
      sample :elec_blup  # after which play the elec blup sample
    end
  end",

  "
  in_thread do
    loop do
      cue :tick, foo: 64  # sending tick heartbeat messages with a value :foo
      sleep 0.5
    end
  end

  # The value for :foo can now be used in synced threads

  loop do
    values = sync :tick
    play values[:foo]    # play the note value from :foo
  end",
      ]


      def __osc_match(matcher_path, osc_path)
        # returns true if matcher matches osc_path
        t = 0
        p = 0
        i = 0
        d = 0
        b = 0
        m = 60
        v = []
        n = matcher_path
        ce = CueEvent.new(t, p, i, d, b, m, n, v)
        SonicPi::EventMatcher.new(ce).path_match(osc_path)
      end

      def get_event(*args)

        if args.empty?
          lookup = lambda { |*args| get_event(*args) }
          return TimeStateLookup.new(lookup)
        else
          k, default = args
          k = __sync_path(k)

          # If we've time_warped into the future raise a timing exception
          if __system_thread_locals.get(:sonic_pi_spider_in_time_warp)

            if __system_thread_locals.get(:sonic_pi_spider_time_warp_start) < __system_thread_locals.get(:sonic_pi_spider_time)
              raise TimingError, "Sadly, you may not time_warp into the future to call get, then bring the result back in time to now."
            end
          end

          cache = __system_thread_locals.get(:sonic_pi_spider_time_state_cache, [])

          match_idx = cache.find_index { |x|  __osc_match(k, x[0]) }
          if match_idx
            return cache[match_idx][1]
          end


          t = __system_thread_locals.get(:sonic_pi_spider_time)
          b = __system_thread_locals.get(:sonic_pi_spider_beat)
          i = __current_thread_id
          d = __system_thread_locals.get(:sonic_pi_spider_thread_delta)
          p = __system_thread_locals.get(:sonic_pi_spider_thread_priority, 1001)
          m = current_bpm

          res = @event_history.get(t, p, i, d, b, m, k)
        end
      end

      def get(*args)
        if args.empty?
          lookup = lambda { |*args| get(*args) }
          return TimeStateLookup.new(lookup)
        else

          params, opts = split_params_and_merge_opts_array(args)
          default = params[1] || opts[:default]
          res = get_event(params[0])
          return res.val if res
          return default
        end
      end
      doc name:           :get,
          introduced:     Version.new(3,0,0),
          summary:        "Get information from the Time State",
          doc:            "Retrieve information from Time State set prior to the current time from either the current or any other thread. If called multiple times will always return the same value unless a call to `sleep`, `sync`, `set` or `cue` is interleaved. Also, calls to `get` will always return the same value across Runs for deterministic behaviour - which means you may safely use it in your compositions for repeatable music.

May be used within a `time_warp` to retrieve past events. If in a time warp, `get` can not be called from a future position. Does not advance time.",
          args:           [[:time_state_key, :default]],
          accepts_block:  false,
          examples:       ["
  get :foo #=> returns the last value set as :foo, or nil",

        "
set :foo, 3
get[:foo] #=> returns 3",

        "
in_thread do
  set :foo, 3
end

in_thread do
  puts get[:foo]  #=> always returns 3 (no race conditions here!)
end
"]






      def current_beat
        beat
      end


      def with_swing (*args, &blk)
        raise ArgumentError, "with_swing must be called with a do/end block." unless blk
        params, opts = split_params_and_merge_opts_array(args)
        shift = params[0] || opts.fetch(:shift, 0.1)

        pulse = params[1] || opts.fetch(:pulse, 4)
        key = (params[2] || opts.fetch(:tick, :swing)).to_sym

        raise ArgumentError, "with_swing shift should be a number. Got: #{shift.inspect}" unless shift.is_a?(Numeric)
        raise ArgumentError, "with_swing pulse should be a positive number. Got: #{pulse.inspect}" unless pulse.is_a?(Numeric) && pulse > 0

        use_shift = (tick(key) % pulse) == 0
        if use_shift
          time_warp shift do
            blk.call
          end
        else
          blk.call
        end
        nil
      end
      doc name:           :with_swing,
          introduced:     Version.new(3,0,0),
          summary:        "Add swing to successive calls to do/end block",
          args:           [[:shift, :beats], [:pulse, :number], [:tick, :symbol]],
          returns:        nil,
          opts:           {shift: "How much time to delay/forward the block. Greater values produce more emphasised swing. Defaults to 0.1 beats.",
                           pulse: "How often to apply the swing. Defaults to 4.",
                           tick: "A key for the tick with which to count pulses. Override this if you have more than one `with_swing` block in your `live_loop` or thread to stop them interfering with each other.",
                           offset: "Count offset - before modding the count with the pulse size - integer offset to add to the result of calling `tick` with the specified tick key (via the `tick:` opt)"},
          accepts_block:  false,
          doc:            "Runs block within a `time_warp` except for once every `pulse` consecutive runs (defaulting to 4). When used for rhythmical purposes this results in one in every `pulse` calls of the block being 'on beat' and the rest shifted forward or backwards in time by `shift` beats.",
          examples: ["
live_loop :foo do
  with_swing 0.1 do
    sample :elec_beep      # plays the :elec_beep sample late except for every 4th time
  end
  sleep 0.25
end
",
        "
live_loop :foo do
  with_swing -0.1 do
    sample :elec_beep      # plays the :elec_beep sample slightly early
  end                      # except for every 4th time
  sleep 0.25
end
",
        "
live_loop :foo do
  with_swing -0.1, pulse: 8 do
    sample :elec_beep      # plays the :elec_beep sample slightly early
  end                      # except for every 8th time
  sleep 0.25
end
",
        "
# Use unique tick names if you plan on using with_swing
# more than once in any given live_loop or thread.
live_loop :foo do
  with_swing 0.14, tick: :a do
    sample :elec_beep      # plays the :elec_beep sample slightly late
  end                      # except for every 4th time

  with_swing -0.1, tick: :b do
    sample :elec_beep, rate: 2  # plays the :elec_beep sample at double rate
  end                           #  slightly early except for every 4th time
  sleep 0.25
end",
        "
live_loop :foo do
  with_swing 0.1 do
    cue :tick              # send out cue messages with swing timing
  end
  sleep 0.25
end

live_loop :bar do
  sync :tick
  sample :elec_beep       # sync on the swing cue messages to bring the swing into
                          # another live loop (sync will match the timing and clock of
                          # the sending live loop)
end
"      ]




      def run_file(path)
        path = File.expand_path(path.to_s)
        raise IOError, "Unable to run file - no file found with path: #{path}" unless File.exist?(path)
        __spider_eval(File.read(path))
      end
      doc name:           :run_file,
          introduced:     Version.new(2,11,0),
          summary:        "Evaluate the contents of the file as a new Run",
          args:           [[:filename, :path]],
          returns:        nil,
          opts:           nil,
          accepts_block:  false,
          doc:            "Reads the full contents of the file with `path` and executes it in a new Run. This works as if the code in the file was in a buffer and Run button was pressed.",
          examples: ["
run_file \"~/path/to/sonic-pi-code.rb\" #=> will run the contents of this file"]

      def run_code(code)
        __spider_eval(code.to_s)
      end
      doc name:           :run_code,
          introduced:     Version.new(2,11,0),
          summary:        "Evaluate the code passed as a String as a new Run",
          args:           [[:code, :string]],
          returns:        nil,
          opts:           nil,
          accepts_block:  false,
          doc:            "Executes the code passed as a string in a new Run. This works as if the code was in a buffer and Run button was pressed.",
          examples: ["
run_code \"sample :ambi_lunar_land\" #=> will play the :ambi_lunar_land sample",

        "# Works with any amount of code:
run_code \"8.times do\nplay 60\nsleep 1\nend # will play 60 8 times"]


      def eval_file(path)
        path = File.expand_path(path.to_s)
        raise IOError, "Unable to run file - no file found with path: #{path}" unless File.exist?(path)
        eval(File.read(path))
      end
      doc name:           :eval_file,
          introduced:     Version.new(3,2,0),
          summary:        "Evaluate the contents of the file inline in the current thread like a function.",
          args:           [[:filename, :path]],
          returns:        nil,
          opts:           nil,
          accepts_block:  false,
          doc:            "Reads the full contents of the file with `path` and executes within the current thread like a function call.",
          examples: ["
eval_file \"~/path/to/sonic-pi-code.rb\" #=> will run the contents of this file"]




      def use_osc_logging(v, &block)
        raise DeprecationError, "use_osc_logging does not work with a do/end block. Perhaps you meant with_osc_logging" if block
        __thread_locals.set(:sonic_pi_suppress_osc_logging, !v)
      end
      doc name:          :use_osc_logging,
          introduced:    Version.new(3,0,0),
          summary:       "Enable and disable OSC logging",
          doc:           "Enable or disable log messages created on OSC functions. This does not disable the OSC functions themselves, it just stops them from being printed to the log",
          args:          [[:true_or_false, :boolean]],
          opts:          nil,
          accepts_block: false,
          examples:      ["use_osc_logging true # Turn on OSC logging", "use_osc_logging false # Disable OSC logging"]




      def with_osc_logging(v, &block)
        raise ArgumentError, "with_osc_logging requires a do/end block. Perhaps you meant use_osc_logging" unless block
        current = __thread_locals.get(:sonic_pi_suppress_osc_logging)
        __thread_locals.set(:sonic_pi_suppress_osc_logging, !v)
        block.call
        __thread_locals.set(:sonic_pi_suppress_osc_logging, current)
      end
      doc name:          :with_osc_logging,
          introduced:    Version.new(3,0,0),
          summary:       "Block-level enable and disable OSC logging",
          doc:           "Similar to use_osc_logging except only applies to code within supplied `do`/`end` block. Previous OSC log value is restored after block.",
          args:          [[:true_or_false, :boolean]],
          opts:          nil,
          accepts_block: true,
          requires_block: true,
          examples:      ["
  # Turn on OSC logging:
  use_osc_logging true

  osc \"/foo\" #  message is printed to log

  with_osc_logging false do
    #OSC logging is now disabled
    osc \"/foo\" # OSC message *is* sent but not displayed in log
  end
  sleep 1
  # Debug is re-enabled
  osc \"/foo\" # message is displayed in log
  "]




      def use_osc(host, port=4559)
        host = host.to_s.strip
        host_and_port = (host.include? ":") ? host : (host + ":" + port.to_s)

        __thread_locals.set :sonic_pi_osc_client, host_and_port.freeze
      end
      doc name:           :use_osc,
          introduced:     Version.new(3,0,0),
          summary:        "Set the default hostname and port number for outgoing OSC messages.",
          args:           [[:hostname, :string], [:port, :number]],
          returns:        nil,
          opts:           nil,
          accepts_block:  false,
          doc:            "Sets the destination host and port that `osc` will send messages to. If no port number is specified - will default to port 4559 (Sonic Pi's default OSC listening port).

OSC (Open Sound Control) is a simple way of passing messages between two separate programs on the same computer or even on different computers via a local network or even the internet. `use_osc` allows you to specify which computer (`hostname`) and program (`port`) to send messages to.

It is possible to send messages to the same computer by using the host name `\"localhost\"`

This is a thread-local setting - therefore each thread (or live loop) can have their own separate `use_osc` values.

Note that calls to `osc_send` will ignore these values.

",
      examples: [
" # Send a simple OSC message to another program on the same machine

use_osc \"localhost\", 7000  # Specify port 7000 on this machine
osc \"/foo/bar\"             # Send an OSC message with path \"/foo/bar\"
                             # and no arguments
",

" # Send an OSC messages with arguments to another program on the same machine

use_osc \"localhost\", 7000        # Specify port 7000 on this machine
osc \"/foo/bar\" 1, 3.89, \"baz\"  # Send an OSC message with path \"/foo/bar\"
                                   # and three arguments:
                                   # 1) The whole number (integer) 1
                                   # 2) The fractional number (float) 3,89
                                   # 3) The string \"baz\"
",

" # Send an OSC messages with arguments to another program on a different machine

use_osc \"10.0.1.5\", 7000         # Specify port 7000 on the machine with address 10.0.1.5
osc \"/foo/bar\" 1, 3.89, \"baz\"  # Send an OSC message with path \"/foo/bar\"
                                   # and three arguments:
                                   # 1) The whole number (integer) 1
                                   # 2) The fractional number (float) 3,89
                                   # 3) The string \"baz\"
",

" # use_osc only affects calls to osc until the next call to use_osc

use_osc \"localhost\", 7000  # Specify port 7000 on this machine
osc \"/foo/bar\"             # Send an OSC message to port 7000
osc \"/foo/baz\"             # Send another OSC message to port 7000

use_osc \"localhost\", 7005  # Specify port 7000 on this machine
osc \"/foo/bar\"             # Send an OSC message to port 7005
osc \"/foo/baz\"             # Send another OSC message to port 7005
",

" # threads may have their own use_osc value

use_osc \"localhost\", 7000  # Specify port 7000 on this machine

live_loop :foo do
  osc \"/foo/bar\"             # Thread inherits outside use_osc values
  sleep 1                      # and therefore sends OSC messages to port 7000
end

live_loop :bar do
  use_osc \"localhost\", 7005  # Override OSC hostname and port for just this
                               # thread (live loop :bar). Live loop :foo is
                               # unaffected.

  osc \"/foo/bar\"             # Send OSC messages to port 7005
  sleep 1
end

use_osc \"localhost\", 7010  # Specify port 7010
osc \"/foo/baz\"             # Send another OSC message to port 7010
                             # Note that neither live loops :foo or :bar
                             # are affected (their use_osc values are
                             # independent and isolated.


",
]

      def with_osc(host, port=4559, &block)
        raise ArgumentError, "with_osc must be called with a do/end block. Perhaps you meant use_osc" unless block
        host = host.to_s.strip
        current_host_and_port = __thread_locals.get(:sonic_pi_osc_client)
        use_osc(host, port)
        res = block.call
        __thread_locals.set(:sonic_pi_osc_client, current_host_and_port)
        res
      end
      doc name:           :with_osc,
          introduced:     Version.new(3,0,0),
          summary:        "Block-level setting for the default hostname and port number of outgoing OSC messages.",
          args:           [[:hostname, :string], [:port, :number]],
          returns:        nil,
          opts:           nil,
          accepts_block:  true,
          doc:            "Sets the destination host and port that `osc` will send messages to for the given do/end block.",
          examples: [
"
use_osc \"localhost\", 7000  # Specify port 7010
osc \"/foo/baz\"             # Send an OSC message to port 7000

with_osc \"localhost\", 7010 do # set hostname and port for the duration
                                # of this do/end block
   osc \"/foo/baz\"             # Send an OSC message to port 7010
end

osc \"/foo/baz\"             # Send an OSC message to port 7000
                             # as old setting is restored outside
                             # do/end block
"        ]

      def __osc_send(host, port, path, *args)
        t = __system_thread_locals.get(:sonic_pi_spider_time) + current_sched_ahead_time
        args.map! do |arg|
          case arg
          when Numeric, String
            arg
          else
            arg.inspect
          end
        end
        @osc_server.send_ts(t, "localhost", @osc_router_port, "/send_after", host, port, path, *args)
      end


      def osc_send(host, port, path, *args)
        host = host.to_s.strip
        __osc_send(host, port, path, *args)
        __delayed_message "OSC -> #{host}, #{port}, #{path}, #{args}" unless __thread_locals.get(:sonic_pi_suppress_osc_logging)
      end
      doc name:           :osc_send,
          introduced:     Version.new(3,0,0),
          summary:        "Send an OSC message to a specific host and port",
          args:           [[:hostname, :string], [:port, :number], [:path, :osc_path], [:args, :list]],
          returns:        nil,
          opts:           nil,
          accepts_block:  true,
          doc:            "Similar to `osc` except ignores any `use_osc` settings and sends the OSC message directly to the specified `hostname` and `port`.

See `osc` for more information.",
          examples: [
"
osc_send \"localhost\", 7000, \"/foo/baz\"  # Send an OSC message to port 7000 on the same machine
",
"
use_osc \"localhost\", 7010                 # set hostname and port
osc \"/foo/baz\"                            # Send an OSC message to port 7010

osc_send \"localhost\", 7000, \"/foo/baz\"  # Send an OSC message to port 7000
                                            # (ignores use_osc settings)

"        ]


      def osc(path, *args)
        host_and_port = __thread_locals.get :sonic_pi_osc_client
        raise ArgumentError, "Please specify a destination with use_osc or with_osc" unless host_and_port
        path = "/#{path}" if path.is_a? Symbol
        host, port = host_and_port.split ":"
        port = port.to_i
        __osc_send host, port, path, *args
        __delayed_message "OSC -> #{host}, #{port}, #{path}, #{args}" unless __thread_locals.get(:sonic_pi_suppress_osc_logging)
      end
      doc name:           :osc,
          introduced:     Version.new(3,0,0),
          summary:        "Send an OSC message (Open Sound Control)",
          args:           [[:path, :arguments]],
          returns:        nil,
          opts:           nil,
          accepts_block:  false,
          doc:            "Sends an OSC message to the current host and port specified by `use_osc` or `with_osc`.

OSC (Open Sound Control) is a simple way of passing messages between two separate programs on the same computer or even on different computers via a local network or even the internet. `osc` enables you to send well-timed OSC messages from within Sonic Pi. `osc` will ensure that the OSC message is sent at the correct time using the same timing system shared with the synthesis functionality via `sample`, `synth` and friends. `osc` even works seamlessly within `time_warp` - see examples.

A typical OSC message has two parts: a descriptive `path` which looks simalar to a URL (website address), and an optional list of `arguments` that are either numbers or strings.

For example, a hypothetical synth program might accept this OSC message:

`/set/filter lowpass 80 0.5`

where `/set/filter` is the path, and `lowpass`, `80`, and `0.5` are three
arguments. This can be sent from within Sonic Pi by writing:

`osc \"/set/filter\", \"lowpass\", 80, 0.5`

However, in order to send the OSC message you must first specify where to send it to. This is achieved by specifying both the host (the machine's internet address) and the port that the remote OSC server is listening on. This is configured using `use_osc` or `with_osc`. So, if our synth program was running on a machine on the local network with IP address `10.0.1.5` on port `5100` we could send our OSC message to it with the following:


`use_osc \"10.0.1.5\", 5100`

`osc \"/set/filter\", \"lowpass\", 80, 0.5`


Note, by default, Sonic Pi listens for OSC messages on port `4559`, so you may send messages to an external machine running Sonic Pi if you know the IP address of that external machine. Any OSC messages received on port `4559` are automatically converted to standard cue events and displayed in the GUI's cue log. This also means that you can use `sync` to wait for the next incoming OSC message with a given path (see example).

Finally, it is also very useful to send OSC messages to aother programs on the same computer. This can be achieved by specifying \"localhost\" as the hostname and the port as normal (depending on which port the other program is listening on).

See `osc_send` for a version which allows you to specify the hostname and port directly (ignoring any values set via `use_osc` or `with_osc`).

For further information see the OSC spec: [http://opensoundcontrol.org/spec-1_0](http://opensoundcontrol.org/spec-1_0)
",
      examples: [
" # Send a simple OSC message to another program on the same machine

use_osc \"localhost\", 7000  # Specify port 7000 on this machine
osc \"/foo/bar\"             # Send an OSC message with path \"/foo/bar\"
                             # and no arguments
",

" # Send an OSC messages with arguments to another program on the same machine

use_osc \"localhost\", 7000        # Specify port 7000 on this machine
osc \"/foo/bar\", 1, 3.89, \"baz\" # Send an OSC message with path \"/foo/bar\"
                                   # and three arguments:
                                   # 1) The whole number (integer) 1
                                   # 2) The fractional number (float) 3.89
                                   # 3) The string \"baz\"
",

" # Send an OSC messages with arguments to another program on a different machine

use_osc \"10.0.1.5\", 7000         # Specify port 7000 on the machine with address 10.0.1.5
osc \"/foo/bar\", 1, 3.89, \"baz\" # Send an OSC message with path \"/foo/bar\"
                                   # and three arguments:
                                   # 1) The whole number (integer) 1
                                   # 2) The fractional number (float) 3.89
                                   # 3) The string \"baz\"
",

" # OSC messages honour the timing system

osc \"/foo/bar\"       # Send an OSC message with path /foo/bar at *exactly* the
play 60                # same time as note 60 is played

sleep 1                # Wait for 1 beat

osc \"/baz/quux\"       # Send an OSC message with path /baz/quux at *exactly* the
play 72                 # same time as note 72 is played
",

" # Send a incrementing OSC counter

live_loop :foo do             # Start a live loop called :foo
  osc \"/counter\", tick      # Send an OSC message with the path /counter
                              # with successive whole numbers (0, 1, 2, 3.. etc.)
                              # each time round the live loop
  sleep 1                     # Repeat the live loop every 1 beat
end
",

" # OSC messages can be sent from within time_warp

time_warp 0.5 do
  osc \"/foo/bar\"       # Send an OSC message with path /foo/bar at 0.5 beats
end

sleep 1                  # Wait for 1 beat

time_warp -0.1 do
  osc \"/baz/quux\"      # Send an OSC message with path /baz/quux at 0.9 beats
end
"

]

      def reset
        __thread_locals.reset!
      end
      doc name:           :reset,
          introduced:     Version.new(2,11,0),
          summary:        "Reset all thread locals",
          args:           [[]],
          returns:        nil,
          opts:           nil,
          accepts_block:  false,
          doc:            "All settings such as the current synth, BPM, random stream and tick values will be reset to the values inherited from the parent thread. Consider using `clear` to reset all these values to their defaults.",
         examples: ["
# Basic Reset
use_synth :blade
use_octave 3

puts \"before\"         #=> \"before\"
puts current_synth      #=> :blade
puts current_octave     #=> 3
puts rand               #=> 0.75006103515625
puts tick               #=> 0

reset

puts \"after\"          #=> \"after\"
puts current_synth      #=> :beep
puts current_octave     #=> 0
puts rand               #=> 0.75006103515625
puts tick               #=> 0",

"Reset remembers defaults from when the thread was created:
use_synth :blade
use_octave 3

puts \"before\"         #=> \"before\"
puts current_synth      #=> :blade
puts current_octave     #=> 3
puts rand               #=> 0.75006103515625
puts tick               #=> 0

at do
  use_synth :tb303
  puts rand               #=> 0.9287109375
  reset
  puts \"thread\"          #=> \"thread\"


                          # The call to reset ensured that the current
                          # synth was returned to the the state at the
                          # time this thread was started. Thus any calls
                          # to use_synth between this line and the start
                          # of the thread are ignored
  puts current_synth      #=> :blade
  puts current_octave     #=> 3

                          # The call to reset ensured
                          # that the random stream was reset
                          # to the same state as it was when
                          # the current thread was started
  puts rand               #=> 0.9287109375
  puts tick               #=> 0
end"]

      def clear
        __thread_locals.clear!
        __set_default_user_thread_locals!
      end
      doc name:           :clear,
          introduced:     Version.new(2,11,0),
          summary:        "Clear all thread locals to defaults",
          args:           [[]],
          returns:        nil,
          opts:           nil,
          accepts_block:  false,
      doc:            "All settings such as the current synth, BPM, random stream and tick values will be reset to their defaults. Consider using `reset` to reset all these values to those inherited from the parent thread.",
      examples: [
"Clear wipes out the threads locals
use_synth :blade
use_octave 3

puts \"before\"         #=> \"before\"
puts current_synth      #=> :blade
puts current_octave     #=> 3
puts rand               #=> 0.75006103515625
puts tick               #=> 0

at do
  use_synth :tb303
  puts rand               #=> 0.9287109375
  clear
  puts \"thread\"         #=> \"thread\"


                          # The clear reset the current synth to the default
                          # of :beep. We are therefore ignoring any inherited
                          # synth settings. It is as if the thread was a completely
                          # new Run.
  puts current_synth      #=> :beep

                          # The current octave defaults back to 0
  puts current_octave     #=> 0

                          # The random stream defaults back to the standard
                          # stream used by every new Run.
  puts rand               #=> 0.75006103515625
  puts tick               #=> 0
end"
]

      def time_warp(times=0, params=nil, &block)
        __schedule_delayed_blocks_and_messages!


        raise ArgumentError, "time_warp requires a do/end block" unless block
        prev_ctl_deltas = __system_thread_locals.get(:sonic_pi_local_control_deltas)
        prev_cache = __system_thread_locals.get(:sonic_pi_spider_time_state_cache, [])
        had_params = params
        times = [times] if times.is_a? Numeric

        # When no params are specified, pass the times through as params
        params ||= times
        params_size = params.size

        raise ArgumentError, "params needs to be a list-like thing" unless params.respond_to? :[]
        raise ArgumentError, "times needs to be a list-like thing" unless times.respond_to? :each_with_index

        vt_orig = __system_thread_locals.get :sonic_pi_spider_time
        density = __thread_locals.get(:sonic_pi_local_spider_density) || 1.0
        orig_sleep_mul_w_density = __system_thread_locals.get(:sonic_pi_spider_sleep_mul) * density
        orig_beat = __system_thread_locals.get(:sonic_pi_spider_beat)
        sat = current_sched_ahead_time
        already_in_time_warp = __system_thread_locals.get :sonic_pi_spider_in_time_warp

        __system_thread_locals.set(:sonic_pi_spider_time_warp_start, vt_orig.freeze) unless  already_in_time_warp
        __system_thread_locals.set_local :sonic_pi_spider_in_time_warp, true

        times.each_with_index do |delta, idx|
          sleep_time = delta * orig_sleep_mul_w_density
          new_time = vt_orig + sleep_time

          raise TimeTravelError, "Time travel error - a jump back of #{delta} is too far.\nSorry, although it would be amazing, you can't go back in time beyond the sched_ahead time of #{sat}" if (Time.now - sat) > new_time

          __change_time!(new_time)
          __system_thread_locals.set :sonic_pi_spider_beat, orig_beat + delta
          __system_thread_locals.set_local :sonic_pi_local_control_deltas, {}
          __system_thread_locals.set_local(:sonic_pi_spider_time_state_cache, [])

          case block.arity
          when 0
            block.call
          when 1
            block.call(params[idx % params_size])
          when 2
            if had_params
              block.call(delta, params[idx % params_size])
            else
              block.call(delta, idx)
            end
          when 3
            block.call(t, params[idx % params_size], idx)
          else
            raise ArgumentError, "block for time_warp should only accept 0, 1, 2 or 3 parameters. You gave: #{block.arity}."
          end
          __schedule_delayed_blocks_and_messages!
        end

        __change_time!(vt_orig)
        __system_thread_locals.set :sonic_pi_spider_beat, orig_beat
        __system_thread_locals.set_local :sonic_pi_spider_in_time_warp, already_in_time_warp
        __system_thread_locals.set_local :sonic_pi_local_control_deltas, prev_ctl_deltas
        __system_thread_locals.set_local(:sonic_pi_spider_time_state_cache, prev_cache)
      end
      doc name:           :time_warp,
          introduced:     Version.new(2,11,0),
          summary:        "Shift time forwards or backwards for the given block",
          args:           [[:delta_time, :number]],
          returns:        nil,
          opts:           nil,
          accepts_block:  true,
          doc:            "The code within the given block is executed with the specified delta time shift specified in beats. For example, if the delta value is 0.1 then all code within the block is executed with a 0.1 beat delay. Negative values are allowed which means you can move a block of code *backwards in time*. For example a delta value of -0.1 will execute the code in the block 0.1 beats ahead of time. The time before the block started is restored after the execution of the block.

Given a list of times, run the block once after waiting each given time. If passed an optional params list, will pass each param individually to each block call. If size of params list is smaller than the times list, the param values will act as rings (rotate through). If the block is given 1 arg, the times are fed through. If the block is given 2 args, both the times and the params are fed through. A third block arg will receive the index of the time.

Note that the code within the block is executed synchronously with the code before and after, so all thread locals will be modified inline - as is the case for `with_fx`. However, as time is always restored to the value before `time_warp` started, you can use it to schedule events for the future in a similar fashion to a thread (via `at` or `in_thread`) without having to use an entirely fresh and distinct set of thread locals - see examples.

Also, note that you cannot travel backwards in time beyond the `current_sched_ahead_time`.

If the `time_warp` block is within a `density` block, the delta time is not affected (although all the other times such as sleep and phase durations will be affected) - see example.

`time_warp` is ahead-of-time scheduling within the current thread. See `at` for just-in-time scheduling using multiple isolated threads.",
          examples:       ["# shift forwards in time
play 70            #=> plays at time 0
sleep 1
play 75            #=> plays at time 1

time_warp 0.1 do
                   # time shifts forward by 0.1 beats
  play 80          #=> plays at 1.1
  sleep 0.5
  play 80          #=> plays at 1.6

end                # time shifts back by 0.6 beats

                   # we now honour the original sleep 1 and the
                   # sleep 0.5 within the time_warp block is
                   # ignored including the 0.1 shift offset

play 70            #=> plays at 1",

        "# shift backwards in time

play 70            #=> plays at time 0
sleep 1
play 75            #=> plays at time 1

time_warp -0.1 do
                   # time shifts backwards by 0.1 beats
  play 80          #=> plays at 0.9
  sleep 0.5
  play 80          #=> plays at 1.4
                   # time shifts forward by 0.1 beats
end
                   # we now honour the original sleep 1 and the
                   # sleep 0.5 within the time_warp block is
                   # ignored, including the -0.1 offset
play 70            #=> plays at 1",

        "# Ticks count linearly through time_warp

puts tick          #=> prints 0 (at time 0)

sleep 1

time_warp 2 do
  puts tick        #=> prints 1 (at time 3)
end

sleep 0.5

puts tick          #=> prints 2 (at time 1.5)",

                "# Comparing time_warp with at

puts tick          #=> prints 0 (at time 0)
sleep 0.5
puts tick          #=> prints 1 (at time 0.5)

time_warp 2 do
  puts tick        #=> prints 2 (at time 2.5)
  sleep 0.5
  puts tick        #=> prints 3 (at time 3)
end

at 3 do            # the at will reset all thread locals
  puts tick        #=> prints 0 (At time 3.5)
  sleep 0.5
  puts tick        #=> prints 1 (At time 4)
end

sleep 0.5

puts tick          #=> prints 4 (at time 1)",

        "# Time Warp within Density
density 2 do                        # Typically this will double the BPM and affect all times
                                    # in addition to looping the internal block twice
  time_warp 0.5 do                  # However, this time is *not* affected and will remain 0.5
    with_fx :slicer, phase: 0.5 do  # This phase duration *is* affected and will be 0.25
      play 60
      sleep 1                       # This time *will* be affected by the density and be 0.5
    end
  end

end

",

        " # Time Warp with lists of times

time_warp [0, 1, 2, 3] do
  puts \"hello\"                # Will print \"hello\" at 0, 1, 2, and 3 seconds
end
                                # Notice that the run completes before all the
                                # messages have been delivered. This is because it
                                # schedules all the messages at once so the program
                                # can complete immediately. This is unlike at which
                                # would appear to behave similarly, but would wait
                                # for all messages to be delivered (on time) before
                                # allowing the program to complete. ",

"time_warp [1, 2, 4] do  # plays a note after waiting 1 beat,
    play 75                # then after 1 more beat,
  end                      # then after 2 more beats (4 beats total)
  ",
  "
  time_warp [1, 2, 3], [75, 76, 77] do |n|  # plays 3 different notes
    play n
  end
  ",
  "
  time_warp [1, 2, 3],
      [{:amp=>0.5}, {:amp=> 0.8}] do |p| # alternate soft and loud
    sample :drum_cymbal_open, p          # cymbal hits three times
  end
  ",
  "
  time_warp [0, 1, 2] do |t| # when no params are given to at, the times are fed through to the block
    puts t #=> prints 0, 1, then 2
  end
  ",
  "
  time_warp [0, 1, 2], [:a, :b] do |t, b|  # If you specify the block with 2 args, it will pass through both the time and the param
    puts [t, b] #=> prints out [0, :a], [1, :b], then [2, :a]
  end
  ",
  "
  time_warp [0, 0.5, 2] do |t, idx|  # If you specify the block with 2 args, and no param list to at, it will pass through both the time and the index
    puts [t, idx] #=> prints out [0, 0], [0.5, 1], then [2, 2]
  end
  ",
  "
  time_warp [0, 0.5, 2], [:a, :b] do |t, b, idx|  # If you specify the block with 3 args, it will pass through the time, the param and the index
    puts [t, b, idx] #=> prints out [0, :a, 0], [0.5, :b, 1], then [2, :a, 2]
  end
  ",
  " # time_warp consumes & interferes with the outer random stream
puts \"main: \", rand  # 0.75006103515625
rand_back
time_warp 1 do         # the random stream inside the at block is the
                       # same as the one in the outer block
  puts \"time_warp:\", rand # 0.75006103515625
  puts \"time_warp:\", rand # 0.733917236328125
  rand_back           # undo last call to rand
end

sleep 2
puts \"main: \", rand # value is now 0.733917236328125 again
",

"
            # Each block run inherits the same thread locals from the previous one.
            # This means things like the thread local counters can flow through
            # time warp iterations:
time_warp [0, 2] do
            # first time round (after 1 beat) prints:
  puts tick # 0
  puts tick # 1
end
            # second time round (after 2 beats) prints:
            # 2
            # 3
" ]


      def tick_set(*args)
        SonicPi::Core::ThreadLocalCounter.set(*args)
      end
      doc name:           :tick_set,
          introduced:     Version.new(2,6,0),
          summary:        "Set tick to a specific value",
          args:           [[:value, :number]],
          alt_args:       [[[:key, :symbol], [:value, :number]]],
          returns:        :number,
          opts:           nil,
          accepts_block:  false,
          doc:            "Set the default tick to the specified `value`. If a `key` is referenced, set that tick to `value` instead. Next call to `look` will return `value`.",
          examples:       ["
  tick_set 40 # set default tick to 40
  puts look   #=> 40",
  "
  tick_set :foo, 40 # set tick :foo to 40
  puts look(:foo)   #=> 40 (tick :foo is now 40)
  puts look         #=> 0 (default tick is unaffected)

  "
      ]




      def tick_reset(*args)
        SonicPi::Core::ThreadLocalCounter.rm(*args)
      end
      doc name:           :tick_reset,
          introduced:     Version.new(2,6,0),
          summary:        "Reset tick to 0",
          args:           [],
          alt_args:       [[[:key, :symbol]]],
          returns:        :number,
          opts:           nil,
          accepts_block:  false,
          doc:            "Reset default tick to 0. If a `key` is referenced, set that tick to 0 instead. Same as calling tick_set(0)",
          examples:       ["
           # increment default tick a few times
  tick
  tick
  tick
  puts look #=> 2 (default tick is now 2)
  tick_set 0 # default tick is now 0
  puts look #=> 0 (default tick is now 0
  ",
  "
                  # increment tick :foo a few times
  tick :foo
  tick :foo
  tick :foo
  puts look(:foo) #=> 2 (tick :foo is now 2)
  tick_set 0 # default tick is now 0
  puts look(:foo) #=> 2 (tick :foo is still 2)
  tick_set :foo, 0 #  reset tick :foo
  puts look(:foo) #=> 0 (tick :foo is now 0)"
      ]




      def tick_reset_all
        SonicPi::Core::ThreadLocalCounter.reset_all
      end
      doc name:           :tick_reset_all,
          introduced:     Version.new(2,6,0),
          summary:        "Reset all ticks",
          args:           [],
          returns:        :nil,
          opts:           nil,
          accepts_block:  false,
          doc:            "Reset all ticks - default and keyed",
          examples:       ["
  tick      # increment default tick and tick :foo
  tick
  tick :foo
  tick :foo
  tick :foo
  puts look #=> 1
  puts look(:foo) #=> 2
  tick_reset_all
  puts look #=> 0
  puts look(:foo) #=> 0
  "
      ]

      def tick(*args)
        SonicPi::Core::ThreadLocalCounter.tick(*args)
      end
      doc name:           :tick,
          introduced:     Version.new(2,6,0),
          summary:        "Increment a tick and return value",
          args:           [[:key, :symbol]],
          alt_args:       [[[:key, :symbol], [:value, :number]]],
          returns:        :number,
          opts:           {step: "The amount to tick up by. Default is 1.",
                           offset: "Offset to add to index returned. Useful when calling tick on lists, rings and vectors to offset the returned value. Default is 0."},
          accepts_block:  false,
          doc:            "Increment the default tick by 1 and return value. Successive calls to `tick` will continue to increment the default tick. If a `key` is specified, increment that specific tick. If an increment `value` is specified, increment key by that value rather than 1. Ticks are `in_thread` and `live_loop` local, so incrementing a tick only affects the current thread's version of that tick. See `tick_reset` and `tick_set` for directly manipulating the tick vals.",
          examples:       ["
  puts tick #=> 0
  puts tick #=> 1
  puts tick #=> 2
  puts tick #=> 3
  ",
  "
  puts tick(:foo) #=> 0 # named ticks have their own counts
  puts tick(:foo) #=> 1
  puts tick(:foo) #=> 2
  puts tick(:bar) #=> 0 # tick :bar is independent of tick :foo
  ",
  "
  # You can tick by more than increments of 1
  # using the step: opt

  puts tick             #=> 0
  puts tick             #=> 1
  puts tick             #=> 2
  puts tick(step: 2)    #=> 4
  puts tick(step: 2)    #=> 6
  puts tick(step: 10)   #=> 16
  puts tick             #=> 17
  ",
  " # Each_live loop has its own separate ticks
  live_loop :fast_tick do
    puts tick   # the fast_tick live_loop's tick will
    sleep 2     # be updated every 2 seconds
  end

  live_loop :slow_tick do
    puts tick   # the slow_tick live_loop's tick is
    sleep 4     # totally independent from the fast_tick
                # live loop and will be updated every 4
                # seconds
  end
  ",
  "
  live_loop :regular_tick do
    puts tick   # the regular_tick live_loop's tick will
    sleep 1     # be updated every second
  end

  live_loop :random_reset_tick do
    if one_in 3 # randomly reset tick
      tick_reset
      puts \"reset tick!\"
    end
    puts tick   # this live_loop's tick is totally
    sleep 1     # independent and the reset only affects
                # this tick.
  end
  ",
  "
  # Ticks work directly on lists, and will tick through each element
  # However, once they get to the end, they'll return nil
  live_loop :scale do
    play [:c, :d, :e, :f, :g].tick   # play all notes just once, then rests
    sleep 1
  end
  ",
  "
  # Normal ticks interact directly with list ticks
  live_loop :odd_scale do
    tick  # Increment the default tick
    play [:c, :d, :e, :f, :g, :a].tick   # this now play every *other* note just once,
                                         # then rests
    sleep 1
  end
  ",
  "
  # Ticks work wonderfully with rings
  # as the ring ensures the tick wraps
  # round internally always returning a
  # value
  live_loop :looped_scale do
    play (ring :c, :d, :e, :f, :g).tick   # play all notes just once, then repeats
    sleep 1
  end
  ",
  "
  # Ticks work wonderfully with scales
  # which are also rings
  live_loop :looped_scale do
    play (scale :e3, :minor_pentatonic).tick   # play all notes just once, then repeats
    sleep 0.25
  end
  "
      ]




      def look(*args)
        return args[1] if args[1].is_a?(Numeric) && args.size == 1
        SonicPi::Core::ThreadLocalCounter.look(*args)
      end
      doc name:           :look,
          introduced:     Version.new(2,6,0),
          summary:        "Obtain value of a tick",
          args:           [],
          alt_args:       [[[:key, :symbol]]],
          returns:        :number,
          opts:           {offset: "Offset to add to index returned. Useful when calling look on lists, rings and vectors to offset the returned value"},
          accepts_block:  false,
          doc:            "Read and return value of default tick. If a `key` is specified, read the value of that specific tick. Ticks are `in_thread` and `live_loop` local, so the tick read will be the tick of the current thread calling `look`.",
          examples:       ["
  puts look #=> 0
  puts look #=> 0
  puts look #=> 0 # look doesn't advance the tick, it just returns the current value
  ",
  "
  puts look #=> 0 # A look is always 0 before the first tick
  tick # advance the tick
  puts look #=> 0 # Note: a look is still 0 after the first tick.
  tick
  puts look #=> 1
  puts look #=> 1 # making multiple calls to look doesn't affect tick value
  tick
  puts look #=> 2
  ",
  "
  tick(:foo)
  tick(:foo)
  puts look(:foo) #=> 1 (keyed look :foo has been advanced)
  puts look #=> 0 (default look hasn't been advanced)
  puts look(:bar) #=> 0 (other keyed looks haven't been advanced either)
  ",
  "
  # You can call look on lists and rings
  live_loop :foo do
    tick                                      # advance the default tick
    use_synth :beep
    play (scale :e3, :minor_pentatonic).look  # look into the default tick to play all notes in sequence
    sleep 0.5
    use_synth :square
    play (ring :e1, :e2, :e3).look, release: 0.25 # use the same look on another ring
    sleep 0.25
  end
  ",
"
# Returns numbers unchanged if single argument
puts look(0)     #=> 0
puts look(4)     #=> 4
puts look(-4)    #=> -4
puts look(20.3)  #=> 20.3"
      ]




      def stop
        # Schedule messages
        __schedule_delayed_blocks_and_messages!
        raise SonicPi::Stop
      end
      doc name:           :stop,
          introduced:     Version.new(2,5,0),
          summary:        "Stop current thread or run",
          args:           [],
          returns:        nil,
          opts:           nil,
          accepts_block:  false,
          doc:            "Stops the current thread or if not in a thread, stops the current run. Does not stop any running synths triggered previously in the run/thread or kill any existing sub-threads.",
          examples:       ["
  sample :loop_amen #=> this sample is played until completion
  sleep 0.5
  stop                #=> signal to stop executing this run
  sample :loop_garzul #=> this never executes
  ",
  "
  in_thread do
    play 60      #=> this note plays
    stop
    sleep 0.5    #=> this sleep never happens
    play 72      #=> this play never happens
  end

  play 80  #=> this plays as the stop only affected the above thread",

  "
  # Stopping live loops
  live_loop :foo
    sample :bd_haus
    sleep 1
    stop               # live loop :foo will now stop and no longer loop
  end

  live_loop :bar       # live loop :bar will continue looping
    sample :elec_blip
    sleep 0.25
  end"
      ]




      def on(condition, &blk)
        blk.call if truthy?(condition)
      end
      doc name:           :on,
          introduced:     Version.new(2,10,0),
          summary:        "Optionally evaluate block",
          args:           [[:condition, :truthy]],
          returns:        nil,
          opts:           nil,
          accepts_block:  false,
          doc:            "Optionally evaluate the block depending on the truthiness of the supplied condition. The truthiness rules are as follows: all values are seen as true except for: false, nil and 0. Lambdas will be automatically called and the truthiness of their results used.",
      examples:       [
"
on true do
  play 70     #=> will play 70 as true is truthy
end",
"
on 1 do
  play 70     #=> will play 70 as 1 is truthy
end",
"
on 0 do
  play 70     #=> will *not* play 70 as 0 is not truthy
end",
"
on false do
  play 70     #=> will *not* play 70 as false is not truthy
end",
"
on nil do
  play 70     #=> will *not* play 70 as nil is not truthy
end",
"
on lambda{true} do
  play 70     #=> will play 70 as the lambda returns a truthy value
end",
"
on lambda{false} do
  play 70     #=> will *not* play 70 as the lambda does not return a truthy value
end",
"
on lambda{[true, false].choose} do
  play 70     #=> will maybe play 70 depending on the choice in the lambda
end"



      ]




      def bools(*args)
        args.map do |a|
          if (a == 0) || (not a)
            false
          else
            true
          end
        end.ring
      end
      doc name:           :bools,
          introduced:     Version.new(2,2,0),
          summary:        "Create a ring of boolean values",
          args:           [[:list, :array]],
          returns:        :ring,
          opts:           nil,
          accepts_block:  false,
          doc:            "Create a new ring of booleans values from 1s and 0s, which can be easier to write and manipulate in a live setting.",
          examples:       [
        "(bools 1, 0)    #=> (ring true, false)",
        "(bools 1, 0, true, false, nil) #=> (ring true, false, true, false, false)"
      ]




      def stretch(*args)
        raise ArgumentError, "stretch needs an even number of arguments, you passed: #{args.size} - #{args.inspect}" unless args.size.even?
        res = args.each_slice(2).flat_map do |values, num_its|

          if !values.respond_to? :flat_map
            values = [values]
          end
          knit(*values.flat_map{|v| [v, num_its]})
        end
        (res||[]).ring
      end
      doc name:           :stretch,
          introduced:     Version.new(2,6,0),
          summary:        "Stretch a sequence of values",
          args:           [[:list, :anything], [:count, :number]],
          returns:        :ring,
          opts:           nil,
          accepts_block:  false,
          doc:            "Stretches a list of values each value repeated count times. Always returns a ring regardless of the type of the list that is stretched. To preserve type, consider using `.stretch` i.e. `(ramp 1, 2, 3).stretch(2) #=> (ramp 1, 1, 2, 2, 3, 3)`",
          examples:       [
        "(stretch [1,2], 3)    #=> (ring 1, 1, 1, 2, 2, 2)",
        "(stretch [:e2, :c3], 1, [:c2, :d3], 2) #=> (ring :e2, :c3, :c2, :c2, :d3, :d3)"
      ]




      def knit(*args)
        raise ArgumentError, "knit must have a even number of arguments, you passed: #{args.size} - #{args.inspect}" unless args.size.even?
        res = []
        args.each_slice(2) do |val, num_its|
          if num_its > 0
            res = res + ([val] * num_its)
          end
        end
        res.ring
      end
      doc name:           :knit,
          introduced:     Version.new(2,2,0),
          summary:        "Knit a sequence of repeated values",
          args:           [[:value, :anything], [:count, :number]],
          returns:        :ring,
          opts:           nil,
          accepts_block:  false,
          doc:            "Knits a series of value, count pairs to create a ring buffer where each value is repeated count times.",
          examples:       [
        "(knit 1, 5)    #=> (ring 1, 1, 1, 1, 1)",
        "(knit :e2, 2, :c2, 3) #=> (ring :e2, :e2, :c2, :c2, :c2)"
      ]


      # helper for spread
      def redistribute(v1, v2)
        vNew = []
        while v1.length > 0 && v2.length > 0
          a1 = v1.shift
          a2 = v2.shift
          vNew.unshift(a1 + a2)
        end
        if v1.length > 0 then
          [vNew, v1]
        else
          [vNew, v2] # v2 might be empty, but that's fine
        end
      end

      def spread(num_accents, size, *args)
        args_h = resolve_synth_opts_hash_or_array(args)
        beat_rotations = args_h[:rotate]
        res = []
        # if someone requests 8 or more accents in a bar of 8 beats
        # default to filling the output with accents
        # (rotation is futile here)
        if num_accents >= size # changed to >=, so v2 is not empty below
          res = [true] * size
          return res.ring
        end
        
        # new part
        v1 = [[true]] * num_accents
        v2 = [[false]] * (size - num_accents)
        # If v2 not empty (thats given here), call at least once.
        (v1, v2) = redistribute(v1,v2)
        # End condition: v2 empty or has just one element
        while v2.length > 1
          (v1, v2) = redistribute(v1,v2)
        end
        res = (v1 + v2).flatten
        
        if beat_rotations && beat_rotations.is_a?(Numeric)
          beat_rotations = beat_rotations.abs
          while beat_rotations > 0
            beat_rotations -= 1 if res.rotate!.first == true
          end
          res.ring
        else
          res.ring
        end
      end
      doc name:           :spread,
          introduced:     Version.new(2,4,0),
          summary:        "Euclidean distribution for beats",
          args:           [[:num_accents, :number], [:size, :number]],
          returns:        :ring,
          opts:           {rotate: "rotate to the next strong beat allowing for easy permutations of the original rhythmic grouping (see example)"},
          accepts_block:  false,
          doc:            "Creates a new ring of boolean values which space a given number of accents as evenly as possible throughout a bar. This is an implementation of the process described in 'The Euclidean Algorithm Generates Traditional Musical Rhythms' (Toussaint 2005).",
          examples:       [
        "(spread 3, 8)    #=> (ring true, false, false, true, false, false, true, false) a spacing of 332",
        "(spread 3, 8, rotate: 1) #=> (ring true, false, false, true, false, true, false, false) a spacing of 323",
        "
  # Easily create interesting polyrhythmic beats
  live_loop :euclid_beat do
    sample :elec_bong, amp: 1.5 if (spread 3, 8).tick # Spread 3 bongs over 8
    sample :perc_snap, amp: 0.8 if (spread 7, 11).look # Spread 7 snaps over 11
    sample :bd_haus, amp: 2 if (spread 1, 4).look # Spread 1 bd over 4
    sleep 0.125
  end
  ",
  "
  # Spread descriptions from
  # 'The Euclidean Algorithm Generates Traditional Musical Rhythms' (Toussaint 2005).
  (spread 2, 5)  # A thirteenth century Persian rhythm called Khafif-e-ramal.

  (spread 3, 4)  # The archetypal pattern of the Cumbria from Columbia, as well
                 # as a Calypso rhythm from Trinidad

  (spread 3, 5)  # When started on the second onset, is another thirteenth
                 # century Persian rhythm by the name of Khafif-e-ramal, as well
                 # as a Romanian folk-dance rhythm.

  (spread 3, 7)  # A ruchenitza rhythm used in a Bulgarian folk-dance.

  (spread 3, 8)  # The Cuban tresillo pattern

  (spread 4, 7)  # Another Ruchenitza Bulgarian folk-dance rhythm

  (spread 4, 9)  # The Aksak rhythm of Turkey.

  (spread 4, 11) # The metric pattern used by Frank Zappa in his piece Outside Now

  (spread 5, 6)  # Yields the York-Samai pattern, a popular Arab rhythm, when
                 # started on the second onset.

  (spread 5, 7)  # The Nawakhat pattern, another popular Arab rhythm.

  (spread 5, 8)  # The Cuban cinquillo pattern.

  (spread 5, 9)  # A popular Arab rhythm called Agsag-Samai.

  (spread 5, 11) # The metric pattern used by Moussorgsky in Pictures at an
                 # Exhibition

  (spread 5, 12) # The Venda clapping pattern of a South African children's
                 # song.

  (spread 5, 16) # The Bossa-Nova rhythm necklace of Brazil.

  (spread 7, 8)  # A typical rhythm played on the Bendir (frame drum)

  (spread 7, 12) # A common West African bell pattern.

  (spread 7, 16) # A Samba rhythm necklace from Brazil.

  (spread 9, 16) # A rhythm necklace used in the Central African Republic.

  (spread 11, 24) # A rhythm necklace of the Aka Pygmies of Central Africa.

  (spread 13, 24) # Another rhythm necklace of the Aka Pygmies of the upper
                  # Sangha.
  "
      ]




      def range(start, finish, *args)
        start = start.to_f
        finish = finish.to_f
        if is_list_like?(args) && args.size == 1 && args.first.is_a?(Numeric)
          # Allow one optional arg for legacy reasons. Versions earlier
          # than v2.5 allowed: range(1, 10, 2)
          step_size = args.first
          inclusive = false
        else
          args_h = resolve_synth_opts_hash_or_array(args)
          step_size = args_h[:step] || 1.0
          step_size = step_size.to_f
          inclusive = args_h[:inclusive]
        end

        return [].ring if start == finish
        step_size = step_size.abs
        res = []
        cur = start
        if inclusive
          if start < finish
            while cur.round(14) <= finish
              res << cur.round(14)
              cur += step_size
            end
          else
            while cur.round(14) >= finish
              res << cur.round(14)
              cur -= step_size
            end
          end

        else
          if start < finish
            while cur.round(14) < finish
              res << cur.round(14)
              cur += step_size
            end
          else
            while cur.round(14) > finish
              res << cur.round(14)
              cur -= step_size
            end
          end
        end
        res.ring
      end
      doc name:           :range,
          introduced:     Version.new(2,2,0),
          summary:        "Create a ring buffer with the specified start, finish and step size",
          args:           [[:start, :number], [:finish, :number], [:step_size, :number]],
          returns:        :ring,
          opts:           {:step      => "Size of increment between steps; step size.",
                           :inclusive => "If set to true, range is inclusive of finish value"},
          accepts_block:  false,
          memoize: true,
          doc:            "Create a new ring buffer from the range arguments (start, finish and step size). Step size defaults to `1`. Indexes wrap around positively and negatively",
          examples:       [
        "(range 1, 5)    #=> (ring 1, 2, 3, 4)",
        "(range 1, 5, inclusive: true) #=> (ring 1, 2, 3, 4, 5)",
        "(range 1, 5, step: 2) #=> (ring 1, 3)",
        "(range 1, -5, step: 2) #=> (ring 1, -1, -3)",
        "(range 1, -5, step: 2)[-1] #=> -3"
      ]




      def line(start, finish, *args)
        start = start.to_f
        finish = finish.to_f
        return [].ring if start == finish
        args_h = resolve_synth_opts_hash_or_array(args)
        num_slices = args_h[:steps] || 4
        inclusive = args_h[:inclusive]

        raise ArgumentError, "steps: opt for fn line should be a positive non-zero whole number" unless num_slices > 0

        if inclusive
          step_size = (start - finish).abs.to_f / (num_slices - 1)
          range(start.to_f, finish.to_f, inclusive: true,  step: step_size)
        else
          step_size = (start - finish).abs.to_f / num_slices
          range(start.to_f, finish.to_f, step: step_size)
        end
      end
      doc name:           :line,
          introduced:     Version.new(2,5,0),
          summary:        "Create a ring buffer representing a straight line",
          args:           [[:start, :number], [:finish, :number]],
          returns:        :ring,
          opts:           {:steps     => "number of slices or segments along the line",
                           :inclusive => "boolean value representing whether or not to include finish value in line"},
          accepts_block:  false,
          memoize: true,
          doc:            "Create a ring buffer representing a straight line between start and finish of steps elements. Steps defaults to `4`. Indexes wrap around positively and negatively. Similar to `range`.",
          examples:       [
        "(line 0, 4, steps: 4)    #=> (ring 0.0, 1.0, 2.0, 3.0)",
        "(line 5, 0, steps: 5)    #=> (ring 5.0, 4.0, 3.0, 2.0, 1.0)",
        "(line 0, 3, inclusive: true) #=> (ring 0.0, 1.0, 2.0, 3.0)"
      ]




      def halves(start, num_halves=1)
        raise ArgumentError, "Start value for halves needs to be a number, got: #{start.inspect}" unless start.is_a?(Numeric)
        start = start.to_f
        return doubles(start, num_halves * -1) if num_halves < 0
        a = []
        val = start
        num_halves.times do
          a << val
          val /= 2.0
        end
        a.ring
      end
      doc name:           :halves,
          introduced:     Version.new(2,10,0),
          summary:        "Create a ring of successive halves",
          args:           [[:start, :number], [:num_halves, :int]],
          returns:        :ring,
          opts:           nil,
          accepts_block:  false,
          memoize: true,
          doc:            "Create a ring containing the results of successive halving of the `start` value. If `num_halves` is negative, will return a ring of `doubles`.",
          examples:       [
        "(halves 60, 2)  #=> (ring 60, 30)",
        "(halves 120, 3) #=> (ring 120, 60, 30)",
        "(halves 120, 5) #=> (ring 120, 60, 30, 15, 7.5)",
        "(halves 30, -5) #=> (ring 30, 60, 120, 240, 480)"
      ]




      def doubles(start, num_doubles=1)
        raise ArgumentError, "Start value for doubles needs to be a number, got: #{start.inspect}" unless start.is_a?(Numeric)
        return halves(start, num_doubles * -1) if num_doubles < 0
        start = start.to_f
        a = []
        val = start
        num_doubles.times do
          a << val
          val *= 2.0
        end
        a.ring
      end
      doc name:           :doubles,
          introduced:     Version.new(2,10,0),
          summary:        "Create a ring of successive doubles",
          args:           [[:start, :number], [:num_doubles, :int]],
          returns:        :ring,
          opts:           nil,
          accepts_block:  false,
          memoize: true,
          doc:            "Create a ring containing the results of successive doubling of the `start` value. If `num_doubles` is negative, will return a ring of `halves`.",
          examples:       [
        "(doubles 60, 2)  #=> (ring 60, 120)",
        "(doubles 1.5, 3) #=> (ring 1.5, 3, 6)",
        "(doubles 1.5, 5) #=> (ring 1.5, 3, 6, 12, 24)",
        "(doubles 100, -4) #=> (ring 100, 50, 25, 12.5)"
      ]




      def vector(*args)
        SonicPi::Core::SPVector.new(args)
      end
      doc name:           :vector,
          introduced:     Version.new(2,6,0),
          summary:        "Create a vector",
          args:           [[:list, :array]],
          returns:        :vector,
          opts:           nil,
          accepts_block:  false,
          doc:            "Create a new immutable vector from args. Out of range indexes return nil.",
          examples:       [
        "(vector 1, 2, 3)[0] #=> 1",
        "(vector 1, 2, 3)[1] #=> 2",
        "(vector 1, 2, 3)[2] #=> 3",
        "(vector 1, 2, 3)[3] #=> nil",
        "(vector 1, 2, 3)[1000] #=> nil",
        "(vector 1, 2, 3)[-1] #=> nil",
        "(vector 1, 2, 3)[-1000] #=> nil",
      ]




      def ring(*args)
        SonicPi::Core::RingVector.new(args)
      end
      doc name:           :ring,
          introduced:     Version.new(2,2,0),
          summary:        "Create a ring buffer",
          args:           [[:list, :array]],
          returns:        :ring,
          opts:           nil,
          accepts_block:  false,
          doc:            "Create a new immutable ring buffer from args. Indexes wrap around positively and negatively",
          examples:       [
        "(ring 1, 2, 3)[0] #=> 1",
        "(ring 1, 2, 3)[1] #=> 2",
        "(ring 1, 2, 3)[3] #=> 1",
        "(ring 1, 2, 3)[-1] #=> 3",
      ]



      def map(*args)
        if args.size > 1

          raise MapArgError, "There needs to be an even number of args to map. Got: #{args.size}" unless args.size.even?
          m = Hash[*args]
          args = [m]
        end
        SonicPi::Core::SPMap.new(*args)
      end
      doc name:           :map,
          introduced:     Version.new(2,11,0),
          summary:        "Create an immutable map",
          args:           [[:list, :array]],
          returns:        :map,
          opts:           nil,
          accepts_block:  false,
          doc:            "Create a new immutable key/value map from args. ",
          examples:       [
        "(map foo: 1, bar: 2)[:foo] #=> 1",
        "(map foo: 1, bar: 2)[:bar] #=> 2",
        "(map foo: 1, bar: 2)[:quux] #=> nil",
      ]





      def ramp(*args)
        SonicPi::Core::RampVector.new(args)
      end
      doc name:           :ramp,
          introduced:     Version.new(2,6,0),
          summary:        "Create a ramp vector",
          args:           [[:list, :array]],
          returns:        :ramp,
          opts:           nil,
          accepts_block:  false,
          doc:            "Create a new immutable ramp vector from args. Indexes always return first or last value if out of bounds.",
          examples:       [
        "(ramp 1, 2, 3)[0] #=> 1",
        "(ramp 1, 2, 3)[1] #=> 2",
        "(ramp 1, 2, 3)[2] #=> 3",
        "(ramp 1, 2, 3)[3] #=> 3",
        "(ramp 1, 2, 3)[1000] #=> 3",
        "(ramp 1, 2, 3)[-1] #=> 1",
        "(ramp 1, 2, 3)[-1000] #=> 1",
      ]




      def choose(args=nil)
        if args
          args.to_a.choose
        else
          return lambda{|col| col.choose}
        end
      end
      doc name:           :choose,
          introduced:     Version.new(2,0,0),
          summary:        "Random list selection",
          args:           [[:list, :array]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Choose an element at random from a list (array).

If no arguments are given, will return a lambda function which when called takes an argument which will be a list to be chosen from. This is useful for choosing random `onset:` vals for samples

Always returns a single element (or nil)" ,
         examples:       ["
  loop do
    play choose([60, 64, 67]) #=> plays one of 60, 64 or 67 at random
    sleep 1
    play chord(:c, :major).choose #=> You can also call .choose on the list
    sleep 1
  end",
"
# Using choose for random sample onsets
live_loop :foo do
  sample :loop_amen, onset: choose   # choose a random onset value each time
  sleep 0.125
end"]




      def pick(*args)
        if is_list_like?(args[0])
          items = args[0]
          if args[1].is_a? Numeric
            n = args[1]
            args.shift(2)
          else
            n = 1
            args.shift(1)
          end
        else
          items = nil
          if args[0].is_a? Numeric
            n = args[0]
            args.shift(1)
          else
            n = 1
          end
        end

        unless items
          return lambda {|col| col.pick(n, *args)}
        end
        items.pick(n, *args)
      end
      doc name:           :pick,
          introduced:     Version.new(2,10,0),
          summary:        "Randomly pick from list (with duplicates)",
          args:           [[:list, :array], [:n, :number_or_nil]],
          opts:           {:skip => "Number of rands to skip over with each successive pick"},
          accepts_block:  false,
          doc:            "Pick n elements from list or ring. Unlike shuffle, after each element has been picked, it is 'returned' to the list so it may be picked again. This means there may be duplicates in the result. If n is greater than the size of the ring/list then duplicates are guaranteed to be in the result.

If `n` isn't supplied it defaults to the size of the list/ring.

If no arguments are given, will return a lambda function which when called takes an argument which will be a list to be picked from. This is useful for choosing random `onset:` vals for samples.

Always returns a list-like thing (either an array or ring)",
         examples:       ["
puts [1, 2, 3, 4, 5].pick(3) #=> [4, 4, 3]",
"
puts (ring 1, 2, 3, 4, 5).pick(3) #=> (ring 4, 4, 3)",

"
puts (ring 1, 2).pick(5) #=> (ring 2, 2, 1, 1, 1)",
"
puts (ring 1, 2, 3).pick #=> (ring 3, 3, 2)",
"
# Using pick for random sample onsets
live_loop :foo do
  sample :loop_amen, onset: pick   # pick a random onset value each time
  sleep 0.125
end"

      ]




      def inc(n)
        n + 1
      end
      doc name:          :inc,
          introduced:    Version.new(2, 1, 0),
          summary:       "Increment",
          args:          [[:n, :number]],
          opts:          {},
          accepts_block: false,
          doc:           "Increment a number by `1`. Equivalent to `n + 1`",
          examples:     [
        "inc 1 # returns 2",
        "inc -1 # returns 0"]




      def dec(n)
        n - 1
      end
      doc name:          :dec,
          introduced:    Version.new(2, 1, 0),
          summary:       "Decrement",
          args:          [[:n, :number]],
          opts:          {},
          accepts_block: false,
          doc:           "Decrement a number by `1`. Equivalent to `n - 1`",
          examples:     [
        "dec 1 # returns 0",
        "dec -1 # returns -2"]

      def loop(&block)
        raise ArgumentError, "loop needs a block" unless block
        Kernel.loop do
          __system_thread_locals.set(:sonic_pi_spider_synced, false)
          slept = block_slept? do
            block.call
          end
          raise ZeroTimeLoopError, "loop did not sleep or sync!" unless slept or __system_thread_locals.get(:sonic_pi_spider_synced)
        end
      end
      doc name:           :loop,
          introduced:     Version.new(2,0,0),
          summary:        "Repeat do/end block forever",
          doc:            "Given a do/end block, repeats it forever. Note that once the program enters the loop - it will not move on but will instead stay within the loop. Plain loops like this are like black holes - instead of sucking in the light they suck in the program.

The loop must either `sleep` or `sync` each time round otherwise it will stop and throw an error. This is to stop the loop from spinning out of control and locking the system.

For a more powerful, flexible loop built for live coding see `live_loop`.",
          args:           [[]],
          opts:           nil,
          accepts_block:  true,
          requires_block: true,
          async_block:    false,
          examples: ["

play 70       # note 70 is played

loop do
  play 50     # This loop will repeat notes 50 and 62 forever
  sleep 1
  play 62
  sleep 2
end

play 80      # This is *never* played as the program is trapped in the loop above"]




      def live_loop(name=nil, *args, &block)
        raise ArgumentError, "live_loop needs to have a unique name. For example: live_loop :foo" unless name
        raise ArgumentError, "live_loop's name needs to be a string or symbol, got: #{name.inspect}. Example usage: live_loop :foo" unless (name.is_a?(Symbol) || name.is_a?(String))
        ll_name = "live_loop_#{name}".to_sym
        raise ArgumentError, "live_loop #{name.inspect} must be called with a do/end block" unless block

        args_h = resolve_synth_opts_hash_or_array(args)

        sync_sym = args_h[:sync]
        sync_bpm_sym = args_h[:sync_bpm]
        sync_sym = nil if sync_bpm_sym

        raise LiveLockError, "livelock detection - live_loop cannot sync with itself - please choose another sync name for live_loop #{name.inspect}" if name == sync_sym || name == sync_bpm_sym

        delay = args_h[:delay]
        raise ArgumentError, "live_loop's delay: opt must be a number, got #{delay.inspect}" if delay && !delay.is_a?(Numeric)

        if args_h.has_key? :auto_cue
          auto_cue = args_h[:auto_cue]
        else
          auto_cue = true
        end

        case block.arity
        when 0
          define(ll_name) do |a|
            block.call
          end
        when 1
          define(ll_name) do |a|
            block.call(a)
          end
        else
          raise ArgumentError, "Live loop block must only accept 0 or 1 args"
        end

        in_thread(name: ll_name, delay: delay, sync: sync_sym, sync_bpm: sync_bpm_sym) do
          __system_thread_locals.set_local :sonic_pi_local_live_loop_auto_cue, auto_cue
          if args_h.has_key?(:init)
            res = args_h[:init]
          else
            res = 0
          end
          use_random_seed args_h[:seed] if args_h[:seed]
          loop do
            __live_loop_cue name if __system_thread_locals.get :sonic_pi_local_live_loop_auto_cue
            res = send(ll_name, res)
          end
        end

        st = sthread(ll_name)
        __system_thread_locals(st).set_local :sonic_pi_local_live_loop_auto_cue, auto_cue if st
        st
      end
      doc name:           :live_loop,
          introduced:     Version.new(2,1,0),
          summary:        "A loop for live coding",
          args:           [[:name, :symbol]],
          opts:           {:init     => "initial value for optional block arg",
                           :auto_cue => "enable or disable automatic cue (default is true)",
                           :delay    => "Initial delay in beats before the live_loop starts. Default is 0.",
                           :sync     => "Initial sync symbol. Will sync with this symbol before the live_loop starts.",
                           :sync_bpm => "Initial sync symbol. Will sync with this symbol before the live_loop starts. Live loop will also inherit the BPM of the thread which cued the symbol.",
                           :seed     => "override initial random generator seed before starting loop."
      },
          accepts_block:  true,
          requires_block: true,
          async_block:    true,
          intro_fn:       true,
          doc:            "Loop the do/end block forever. However, unlike a basic loop, a live_loop has two special properties. Firstly it runs in a thread - so you can have any number of live loops running at the same time (concurrently). Secondly, you can change the behaviour of a live loop whilst it is still running without needing to stop it. Live loops are therefore the secret to live coding with Sonic Pi.

As live loops are excecuted within a named in_thread, they behave similarly. See the in_thread documentation for all the details. However, it's worth mentioning a few important points here. Firstly, only one live loop with a given name can run at any one time. Therefore, if you define two or more `live_loop`s called `:foo` only one will be running. Another important aspect of `live_loop`s is that they manage their own thread locals set with the `use_*` and `with_*` fns. This means that each `live_loop` can have its own separate default synth, BPM and sample defaults. When a `live_loop` is *first* created, it inherits the thread locals from the parent thread, but once it has started, the only way to change them is by re-defining the do/end body of the `live_loop`. See the examples below for details. Finally, as mentioned above, provided their names are different, you may have many `live_loop`s executing at once.

A typical way of live coding with live loops is to define a number of them in a buffer, hit Run to start them and then to modify their do/end blocks and then hit Run again. This will not create any more thread, but instead just modify the behaviour of the existing threads. The changes will *not* happen immediately. Instead, they will only happen the next time round the loop. This is because the behaviour of each live loop is implemented with a standard function. When a live loop is updated, the function definition is also updated. Each time round the live loop, the function is called, so the new behviour is only observed next time round the loop.

Also sends a `cue` with the same name each time the `live_loop` repeats. This may be used to `sync` with other threads and `live_loop`s.

If the `live_loop` block is given a parameter, this is given the result of the last run of the loop (with initial value either being `0` or an init arg). This allows you to 'thread' values across loops.

Finally, it is possible to delay the initial trigger of the live_loop on creation with both the `delay:` and `sync:` opts. See their respective docstrings. If both `delay:` and `sync:` are specified, on initial live_loop creation first the delay will be honoured and then the sync.
",
          examples:       ["
## Define and start a simple live loop

live_loop :ping do  # Create a live loop called :ping
  sample :elec_ping # This live loops plays the :elec_ping sample
  sleep 1           # Then sleeps for 1 beat before repeating
end
  ",

        "
## Every live loop must sleep or sync

live_loop :ping do  # Create a live loop called :ping
  sample :elec_ping # This live loops plays the :elec_ping sample
                    # However, because the do/end lock of the live loop does not
                    # contain any calls to sleep or sync, the live loop stops at
                    # the end of the first loop with a 'Did not sleep' error.
end",

        "
## Multiple live loops will play at the same time
live_loop :foo do  # Start a live loop called :foo
  play 70
  sleep 1
end

live_loop :bar do  # Start another live loop called :bar
  sample :bd_haus  # Both :foo and :bar will be playing
  sleep 0.5        # at the same time.
end
",

        "
## Live loops inherit external use_* thread locals
use_bpm 30
live_loop :foo do
  play 70           # live loop :foo now has a BPM of 30
  sleep 1           # This sleep will be for 2 seconds
end
",

        "
## Live loops can have their own thread locals
live_loop :foo do
  use_bpm 30       # Set the BPM of live loop :foo to 30
  play 70
  sleep 1          # This sleep will be for 2 seconds
end

live_loop :bar do
  use_bpm 120      # Set the BPM of live loop :bar to 120
  play 82
  sleep 1          # This sleep will be for 0.5 seconds
end
",

        "
## Live loops can pass values between iterations
live_loop :foo do |a|  # pass a param (a) to the block (inits to 0)
  puts a               # prints out all the integers
  sleep 1
  a += 1               # increment a by 1 (last value is passed back into the loop)
end
  ",

        "
## Live loop names must be unique
live_loop :foo do  # Start a live loop called :foo
  play 70
  sleep 1
end

live_loop :foo do  # Attempt to start another also called :foo
  sample :bd_haus  # With a different do/end block
  sleep 0.5        # This will not start another live loop
                   # but instead replace the behaviour of the first.
end                # There will only be one live loop running playing
                   # The bass drum
",
        "
## You can sync multiple live loops together
live_loop :foo, sync: :bar do # Wait for a :bar cue event before starting :foo
 play 70                      # Live loop :foo is therefore blocked and does
 sleep 1                      # not make a sound initially
end

sleep 4                       # Wait for 4 beats

live_loop :bar do             # Start a live loop called :foo which will emit a :bar
  sample :bd_haus             # cue message therefore releasing the :foo live loop.
  sleep 0.5                   # Live loop :foo therefore starts and also inherits the
end                           # logical time of live loop :bar.

                              # This pattern is also useful to re-sync live loops after
                              # errors are made. For example, when modifying live loop :foo
                              # it is possible to introduce a runtime error which will stop
                              # :foo but not :bar (as they are separate, isolated threads).
                              # Once the error has been fixed and the code is re-run, :foo
                              # will automatically wait for :bar to loop round and restart
                              # in sync with the correct virtual clock."

      ]


      def block_duration(&block)
        t1 = __system_thread_locals.get(:sonic_pi_spider_time)
        block.call
        t2 = __system_thread_locals.get(:sonic_pi_spider_time)
        t2 - t1
      end
      doc name:           :block_duration,
          introduced:     Version.new(2,9,0),
          summary:        "Return block duration",
          doc:            "Given a block, runs it and returns the amount of time that has passed. This time is in seconds and is not scaled to the current BPM. Any threads spawned in the block are not accounted for.",
          args:           [[]],
          opts:           nil,
          accepts_block:  true,
          requires_block: true,
          async_block:    false,
          examples: ["
dur = block_duration do
  play 50
  sleep 1
  play 62
  sleep 2
end

puts dur #=> Returns 3 as 3 seconds have passed within the block",
"use_bpm 120
dur = block_duration do
  play 50
  sleep 1
  play 62
  sleep 2
end

puts dur #=> Returns 1.5 as 1.5 seconds have passed within the block
         #   (due to the BPM being 120)"]




      def block_slept?(&block)
        dur = block_duration(&block)
        dur > 0
      end
      doc name:           :block_slept?,
          introduced:     Version.new(2,9,0),
          summary:        "Determine if block contains sleep time",
          doc:            "Given a block, runs it and returns whether or not the block contained sleeps or syncs",
          args:           [[]],
          opts:           nil,
          accepts_block:  true,
          requires_block: true,
          async_block:    false,
          examples: ["
slept = block_slept? do
  play 50
  sleep 1
  play 62
  sleep 2
end

puts slept #=> Returns true as there were sleeps in the block",
"
in_thread do
  sleep 1
  cue :foo  # trigger a cue on a different thread
end

slept = block_slept? do
  sync :foo  # wait for the cue before playing the note
  play 62
end

puts slept #=> Returns true as the block contained a sync.",
"
slept = block_slept? do
  play 50
  play 62
end

puts slept #=> Returns false as there were no sleeps in the block"]



      def at(times=0, params=nil, &block)
        raise ArgumentError, "at must be called with a do/end block" unless block
        had_params = params
        times = [times] if times.is_a? Numeric
        # When no params are specified, pass the times through as params
        params ||= times

        raise ArgumentError, "params needs to be a list-like thing" unless params.respond_to? :[]
        raise ArgumentError, "times needs to be a list-like thing" unless times.respond_to? :each_with_index

        params_size = params.size
        times.each_with_index do |t, idx|
          in_thread do
            sleep t
            case block.arity
            when 0
              block.call
            when 1
              block.call(params[idx % params_size])
            when 2
              if had_params
                block.call(t, params[idx % params_size])
              else
                block.call(t, idx)
              end
            when 3
              block.call(t, params[idx % params_size], idx)
            else
              raise ArgumentError, "block for at should only accept 0, 1, 2 or 3 parameters. You gave: #{block.arity}."
            end
          end
        end
      end
      doc name:           :at,
          introduced:     Version.new(2,1,0),
          summary:        "Asynchronous Time. Run a block at the given time(s)",
          doc:            "Given a list of times, run the block once after waiting each given time. If passed an optional params list, will pass each param individually to each block call. If size of params list is smaller than the times list, the param values will act as rings (rotate through). If the block is given 1 arg, the times are fed through. If the block is given 2 args, both the times and the params are fed through. A third block arg will receive the index of the time.

Note, all code within the block is executed in its own thread. Therefore despite inheriting all thread locals such as the random stream and ticks, modifications will be isolated to the block and will not affect external code.

`at` is just-in-time scheduling using multiple isolated threads. See `time_warp` for ahead-of-time scheduling within the current thread.",
          args:           [[:times, :list],
                           [:params, :list]],
          opts:           nil,
          accepts_block:  true,
          requires_block: true,
          async_block:    true,
          examples:       ["
  at 4 do
    sample :ambi_choir    # play sample after waiting for 4 beats
  end
  ",
  "
  at [1, 2, 4] do  # plays a note after waiting 1 beat,
    play 75           # then after 1 more beat,
  end                 # then after 2 more beats (4 beats total)
  ",
  "
  at [1, 2, 3], [75, 76, 77] do |n|  # plays 3 different notes
    play n
  end
  ",
  "
  at [1, 2, 3],
      [{:amp=>0.5}, {:amp=> 0.8}] do |p| # alternate soft and loud
    sample :drum_cymbal_open, p          # cymbal hits three times
  end
  ",
  "
  at [0, 1, 2] do |t| # when no params are given to at, the times are fed through to the block
    puts t #=> prints 0, 1, then 2
  end
  ",
  "
  at [0, 1, 2], [:a, :b] do |t, b|  #If you specify the block with 2 args, it will pass through both the time and the param
    puts [t, b] #=> prints out [0, :a], [1, :b], then [2, :a]
  end
  ",
  "
  at [0, 0.5, 2] do |t, idx|  #If you specify the block with 2 args, and no param list to at, it will pass through both the time and the index
    puts [t, idx] #=> prints out [0, 0], [0.5, 1], then [2, 2]
  end
  ",
  "
  at [0, 0.5, 2], [:a, :b] do |t, b, idx|  #If you specify the block with 3 args, it will pass through the time, the param and the index
    puts [t, b, idx] #=> prints out [0, :a, 0], [0.5, :b, 1], then [2, :a, 2]
  end
  ",
  " # at does not consume & interfere with the outer random stream
puts \"main: \", rand  # 0.75006103515625
rand_back
at 1 do         # the random stream inside the at block is separate and
                # isolated from the outer stream.
  puts \"at:\", rand # 0.9287109375
  puts \"at:\", rand # 0.1043701171875
end

sleep 2
puts \"main: \", rand # value is still 0.75006103515625
",

"
            # Each block run within at has its own isolated random stream:
at [1, 2] do
            # first time round (after 1 beat) prints:
  puts rand # 0.9287109375
  puts rand # 0.1043701171875
end
            # second time round (after 2 beats) prints:
            # 0.1043701171875
            # 0.764617919921875
"
      ]




      def version
        @version
      end
      doc name:           :version,
          introduced:     Version.new(2,0,0),
          summary:        "Get current version information",
          args:           [],
          opts:           nil,
          accepts_block:  false,
          doc:            "Return information representing the current version of Sonic Pi. This information may be further inspected with `version.major`, `version.minor`, `version.patch` and `version.dev`",
          examples:       ["
  puts version # => Prints out the current version such as v2.0.1",
  "
  puts version.major # => Prints out the major version number such as 2",
  "
  puts version.minor # => Prints out the minor version number such as 0",
  "
  puts version.patch # => Prints out the patch level for this version such as 0"]




      def spark_graph(*values)
        if is_list_like?(values.first) && values.length == 1
          values = values.first
        end

        return "" if values.length == 0
        return "spark error: can't use nested arrays" if Array(values).flatten.length != Array(values).length

        #implementation stolen from @jcromartie https://gist.github.com/jcromartie/1367091
        @ticks = %w[▁ ▂ ▃ ▄ ▅ ▆ ▇]
        values = values.map do |x|
          case x
          when TrueClass
            1
          when FalseClass
            0
          else
            begin
              x.to_f
            rescue NoMethodError
              0
            end
          end
        end
        min = values.min
        range = values.max - values.min
        scale = @ticks.length - 1

        # Guard lists of length 1 or repeating vals
        range = 1.0 if range.to_f == 0.0
        res = String.new("")
        values.map {|x|
          res << @ticks[(((x - min) / range) * scale).round]
        }
        return res
      end
      doc name:           :spark_graph,
          introduced:     Version.new(2,5,0),
          summary:        "Returns a string representing a list of numeric values as a spark graph/bar chart",
          args:           [],
          opts:           nil,
          accepts_block:  false,
          doc:            "Given a list of numeric values, this method turns them into a string of bar heights. Useful for quickly graphing the shape of an array. Remember to use puts so you can see the output. See `spark` for a simple way of printing a spark graph.",
      examples:           [
  "puts (spark_graph (range 1, 5))    #=> ▁▃▅█",
  "puts (spark_graph (range 1, 5).shuffle) #=> ▃█▅▁"
      ]




      def spark(*values)
        puts spark_graph(*values)
      end
      doc name:           :spark,
          hide:           false,
          introduced:     Version.new(2,5,0),
          summary:        "Print a string representing a list of numeric values as a spark graph/bar chart",
          args:           [],
          opts:           nil,
          accepts_block:  false,
          doc:            "Given a list of numeric values, this method turns them into a string of bar heights and prints them out. Useful for quickly graphing the shape of an array.",
          examples:       [
  "spark (range 1, 5)    #=> ▁▃▅█",
  "spark (range 1, 5).shuffle #=> ▃█▅▁"
      ]




      def defonce(name, *opts, &block)
        raise ArgumentError, "defonce must be called with a do/end block" unless block
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
          opts:           {:override => "If set to true, re-definitions are allowed and this acts like define"},
          accepts_block:  true,
          requires_block: true,
          doc:            "Allows you to assign the result of some code to a name, with the property that the code will only execute once - therefore stopping re-definitions. This is useful for defining values that you use in your compositions but you don't want to reset every time you press run. You may force the block to execute again regardless of whether or not it has executed once already by using the override option (see examples).",
          examples:       ["

  defonce :foo do  # Define a new function called foo
    sleep 1        # Sleep for a beat in the function definition. Note that this amount
                   # of time in seconds will depend on the current BPM of the live_loop
                   # or thread calling this function.
    puts \"hello\" # Print hello
    10             # Return a value of 10
  end

  # Call foo on its own
  puts foo # The run sleeps for a beat and prints \"hello\" before returning 10

  # Try it again:
  puts foo # This time the run doesn't sleep or print anything out. However, 10 is still returned.



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




      def ndefine(name, &block)
        # do nothing!
      end
      doc name:           :ndefine,
          introduced:     Version.new(2,1,0),
          summary:        "Define a new function",
          args:           [[:name, :symbol]],
          opts:           nil,
          accepts_block:  true,
          requires_block: true,
          doc:            "Does nothing. Use to stop a define from actually defining. Simpler than wrapping whole define in a comment block or commenting each individual line out.",
          examples:       []




      def define(name, &block)
        raise ArgumentError, "define must be called with a do/end block" unless block
        already_defined = @user_methods.method_defined? name

        if !already_defined && self.respond_to?(name)
          raise ArgumentError, "A function called #{name} is already part of Sonic Pi's core API. Please choose another name."
        end

        if already_defined
          __info "Redefining fn #{name.inspect}"
        else
          __info "Defining fn #{name.inspect}"
        end
        @user_methods.send(:define_method, name, &block)
      end
      doc name:           :define,
          introduced:     Version.new(2,0,0),
          summary:        "Define a new function",
          args:           [[:name, :symbol]],
          opts:           nil,
          accepts_block:  true,
          requires_block: true,
          intro_fn:       true,
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




      def comment(*args, &block)
        raise ArgumentError, "comment requires a block." unless block
        #do nothing!
      end
      doc name:           :comment,
          introduced:     Version.new(2,0,0),
          summary:        "Block level commenting",
          args:           [],
          opts:           nil,
          accepts_block:  true,
          requires_block: true,
          doc:            "Does not evaluate any of the code within the block. However, any optional args passed before the block *will* be evaluated although they will be ignored. See `uncomment` for switching commenting off without having to remove the comment form.",
          examples:       ["
  comment do # starting a block level comment:
    play 50 # not played
    sleep 1 # no sleep happens
    play 62 # not played
  end"]




      def uncomment(*args, &block)
        raise ArgumentError, "uncomment requires a block." unless block
        block.call
      end
      doc name:           :uncomment,
          introduced:     Version.new(2,0,0),
          summary:        "Block level comment ignoring",
          args:           [],
          opts:           nil,
          accepts_block:  true,
          requires_block: true,
          doc:            "Evaluates all of the code within the block. Use to reverse the effect of the comment without having to explicitly remove it.",
          examples:       ["
  uncomment do # starting a block level comment:
    play 50 # played
    sleep 1 # sleep happens
    play 62 # played
  end"]




      def print(*msgs)
        output = msgs.map{|m| m.sp_log_inspect}.join(" ")
        __delayed_user_message output
      end
      doc name:          :print,
          introduced:     Version.new(2,0,0),
          summary:       "Display a message in the output pane",
          args:          [[:output, :anything]],
          opts:          nil,
          accepts_block: false,
          intro_fn:       true,
          doc:           "Displays the information you specify as a string inside the output pane. This can be a number, symbol, or a string itself. Useful for debugging. Synonym for `puts`.",
          examples:      [
  "print \"hello there\"   #=> will print the string \"hello there\" to the output pane",
  "print 5               #=> will print the number 5 to the output pane",
  "print foo             #=> will print the contents of foo to the output pane"]




      def puts(*msgs)
        output = msgs.map{|m| m.sp_log_inspect}.join(" ")
        __delayed_user_message output
      end
      doc name:           :puts,
          introduced:     Version.new(2,0,0),
          summary:        "Display a message in the output pane",
          args:           [[:output, :anything]],
          opts:           nil,
          accepts_block:  false,
          intro_fn:       true,
          doc:            "Displays the information you specify as a string inside the output pane. This can be a number, symbol, or a string itself. Useful for debugging. Synonym for `print`.",
          examples:      [
  "print \"hello there\"   #=> will print the string \"hello there\" to the output pane",
  "print 5               #=> will print the number 5 to the output pane",
  "print foo             #=> will print the contents of foo to the output pane"]




      def vt
        __current_local_run_time
      end
      doc name:           :vt,
          introduced:     Version.new(2,1,0),
          summary:        "Get virtual time",
          args:           [],
          opts:           nil,
          accepts_block:  false,
          doc:            "Get the virtual time of the current thread.",
          examples:      ["
  puts vt # prints 0
   sleep 1
   puts vt # prints 1"]




      def factor?(val, factor)
        return false if factor == 0
        (val % factor) == 0
      end
      doc name:           :factor?,
          introduced:     Version.new(2,1,0),
          summary:        "Factor test",
          args:           [[:val, :number], [:factor, :number]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Test to see if factor is indeed a factor of `val`. In other words, can `val` be divided exactly by factor.",
          examples:       [
  "
  factor?(10, 2) # true - 10 is a multiple of 2 (2 * 5 = 10)
  ",
  "
  factor?(11, 2) #false - 11 is not a multiple of 2
  ",
  "
  factor?(2, 0.5) #true - 2 is a multiple of 0.5 (0.5 * 4 = 2) "
      ]




      def quantise(n, step)
        raise ArgumentError, "quantisation step resolution should be positive" if step <= 0
        (n.to_f / step).round * step
      end
      doc name:           :quantise,
          introduced:     Version.new(2,1,0),
          summary:        "Quantise a value to resolution",
          args:           [[:n, :number], [:step, :positive_number]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Round value to the nearest multiple of step resolution.",
          examples:        [
  "
  quantise(10, 1) # 10 is already a multiple of 1, so returns 10" ,
  "
  quantise(10, 1.1) # Returns 9.9 which is 1.1 * 9",
  "
  quantise(13.3212, 0.1) # 13.3",
  "
  quantise(13.3212, 0.2) # 13.4",
  "
  quantise(13.3212, 0.3) # 13.2",
  "
  quantise(13.3212, 0.5) # 13.5"]




      def dice(num_sides=6)
        rrand_i(1, num_sides)
      end
      doc name:           :dice,
          introduced:     Version.new(2,0,0),
          summary:        "Random dice throw",
          args:           [[:num_sides, :number]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Throws a dice with the specified num_sides (defaults to `6`) and returns the score as a number between `1` and `num_sides`.",
          examples:      ["
  dice # will return a number between 1 and 6 inclusively
       # (with an even probability distribution).",
  "
  dice 3 # will return a number between 1 and 3 inclusively"]




      def one_in(num)
        num = num.to_i
        if num < 1
          false
        else
          rrand_i(1, num) == 1
        end
      end
      doc name:           :one_in,
          introduced:     Version.new(2,0,0),
          summary:        "Random true value with specified probability",
          args:           [[:num, :number]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Returns `true` or `false` with a specified probability - it will return true every one in num times where num is the param you specify",
          examples:       ["
  one_in 2 # will return true with a probability of 1/2, false with probability 1/2",
  "
  one_in 3 # will return true with a probability of 1/3, false with a probability of 2/3",
  "
  one_in 100 # will return true with a probability of 1/100, false with a probability of 99/100"]




      def rdist(width, centre=0, *opts)
        rrand(centre - width, centre + width, *opts)
      end
      doc name:           :rdist,
          introduced:     Version.new(2,3,0),
          summary:        "Random number in centred distribution",
          args:           [[:width, :number], [:centre, :number]],
          alt_args:       [[:width, :number]],
          opts:           {:step => "Step size of value to quantise to."},
          accepts_block:  false,
          doc:            "Returns a random number within the range with width around centre. If optional arg `step:` is used, the result is quantised by step.",
          examples:       [
  "
  print rdist(1, 0) #=> will print a number between -1 and 1
  ",
  "
  print rdist(1) #=> centre defaults to 0 so this is the same as rdist(1, 0)
  ",
  "
  loop do
    play :c3, pan: rdist(1) #=> Will play :c3 with random L/R panning
    sleep 0.125
  end"]




      def rrand(min, max, *opts)
        args_h = resolve_synth_opts_hash_or_array(opts)
        res = args_h[:step]
        if min == max
          if res
            return quantise(min, res)
          else
            return min
          end
        end

        range = (min - max).abs
        r = SonicPi::Core::SPRand.rand!(range)
        smallest = [min, max].min

        if res
          return quantise((r + smallest), res)
        else
          r + smallest
        end
      end
      doc name:           :rrand,
          introduced:     Version.new(2,0,0),
          summary:        "Generate a random float between two numbers",
          args:           [[:min, :number], [:max, :number]],
          opts:           {:step => "Step size of value to quantise to."},
          accepts_block:  false,
          intro_fn:       true,
          doc:            "Given two numbers, this produces a float between the supplied min and max values exclusively. Both min and max need to be supplied. For random integers, see `rrand_i`. If optional arg `step:` is used, the result is quantised by step.",
          examples:       ["
  print rrand(0, 10) #=> will print a number like 8.917730007820797 to the output pane",
  "
  loop do
    play rrand(60, 72) #=> Will play a random non-integer midi note between C4 (60) and C5 (72) such as 67.3453 or 71.2393
    sleep 0.125
  end"]




      def rrand_i(min, max)
        return min if min == max
        range = (min - max).abs
        r = SonicPi::Core::SPRand.rand_i!(range.to_i + 1)
        smallest = [min, max].min
        (r + smallest)
      end
      doc name:           :rrand_i,
          introduced:     Version.new(2,0,0),
          summary:        "Generate a random whole number between two points inclusively",
          args:           [[:min, :number], [:max, :number]],
          opts:           nil,
          accepts_block: false,
          doc:            "Given two numbers, this produces a whole number between the min and max you supplied inclusively. Both min and max need to be supplied. For random floats, see `rrand`",
          examples:      ["
  print rrand_i(0, 10) #=> will print a random number between 0 and 10 (e.g. 4, 0 or 10) to the output pane",
  "
  loop do
    play rrand_i(60, 72) #=> Will play a random midi note between C4 (60) and C5 (72)
    sleep 0.125
  end"]




      def rand(max=1)
        return 0.0 if max == 0
        if max.is_a?(Range)
          rrand(max.min, max.max)
        else
          SonicPi::Core::SPRand.rand!(max)
        end
      end
      doc name:           :rand,
          introduced:     Version.new(2,0,0),
          summary:        "Generate a random float below a value",
          args:           [[:max, :number_or_range]],
          opts:           nil,
          accepts_block:  false,
          intro_fn:       true,
          doc:            "Given a max number, produces a float between `0` and the supplied max value. If max is a range, produces a float within the range. With no args returns a random value between `0` and `1`.",
          examples:       ["
  print rand(0.5) #=> will print a number like 0.375030517578125 to the output pane"]




      def rand_i(max=2)
        return 0 if max == 0
        if max.is_a?(Range)
          rrand_i(max.min, max.max)
        else
          SonicPi::Core::SPRand.rand_i!(max)
        end
      end
      doc name:           :rand_i,
          introduced:     Version.new(2,0,0),
          summary:        "Generate a random whole number below a value (exclusive)",
          args:           [[:max, :number_or_range]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Given a max number, produces a whole number between `0` and the supplied max value exclusively. If max is a range produces an int within the range. With no args returns either `0` or `1`",
          examples:       ["
  print rand_i(5) #=> will print either 0, 1, 2, 3, or 4 to the output pane"]


      def rand_look(*args)
        res = rand(*args)
        rand_back
        res
      end
      doc name:           :rand_look,
          introduced:     Version.new(2,11,0),
          summary:        "Generate a random number without consuming a rand",
          args:           [[:max, :number_or_range]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Given a max number, produces a number between `0` and the supplied max value exclusively. If max is a range produces an int within the range. With no args returns a value between `0` and `1`.

Does not consume a random value from the stream. Therefore, multiple sequential calls to `rand_look` will all return the same value.",
          examples:       ["
  print rand_look(0.5) #=> will print a number like 0.375030517578125 to the output pane",

        "
  print rand_look(0.5) #=> will print a number like 0.375030517578125 to the output pane
  print rand_look(0.5) #=> will print the same number again
  print rand_look(0.5) #=> will print the same number again
  print rand_(0.5) #=> will print a different random number
  print rand_look(0.5) #=> will print the same number as the prevoius line again."
      ]




      def rand_i_look(*args)
        res = rand_i(*args)
        rand_back
        res
      end
      doc name:           :rand_i_look,
          introduced:     Version.new(2,11,0),
          summary:        "Generate a random whole number without consuming a rand",
          args:           [[:max, :number_or_range]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Given a max number, produces a whole number between `0` and the supplied max value exclusively. If max is a range produces an int within the range. With no args returns either `0` or `1`.

Does not consume a random value from the stream. Therefore, multiple sequential calls to `rand_i_look` will all return the same value.",
          examples:       ["
print rand_i_look(5) #=> will print either 0, 1, 2, 3, or 4 to the output pane",

        "
print rand_i_look(5) #=> will print either 0, 1, 2, 3, or 4 to the output pane
print rand_i_look(5) #=> will print the same number again
print rand_i_look(5) #=> will print the same number again
print rand_i(5) #=> will print either 0, 1, 2, 3, or 4 to the output pane
print rand_i_look(5) #=> will print the same number as the previous statement"
      ]




      def rand_back(amount=1)
        SonicPi::Core::SPRand.dec_idx!(amount)
        SonicPi::Core::SPRand.rand_peek
      end
      doc name:           :rand_back,
          introduced:     Version.new(2,7,0),
          summary:        "Roll back random generator",
          args:           [[:amount, :number]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Roll the random generator back essentially 'undoing' the last call to `rand`. You may specify an amount to roll back allowing you to skip back n calls to `rand`.",
          examples:       ["
  # Basic rand stream rollback

  puts rand # prints 0.75006103515625

  rand_back # roll random stream back one
            # the result of the next call to rand will be
            # exactly the same as the previous call

  puts rand # prints 0.75006103515625 again!
  puts rand # prints 0.733917236328125",
  "
  # Jumping back multiple places in the rand stream

  puts rand # prints 0.75006103515625
  puts rand # prints 0.733917236328125
  puts rand # prints 0.464202880859375
  puts rand # prints 0.24249267578125

  rand_back(3) # roll random stream back three places
               # the result of the next call to rand will be
               # exactly the same as the result 3 calls to
               # rand ago.

  puts rand # prints  0.733917236328125 again!
  puts rand # prints  0.464202880859375"]




      def rand_skip(amount=1)
        SonicPi::Core::SPRand.inc_idx!(amount)
        SonicPi::Core::SPRand.rand_peek
      end
      doc name:           :rand_skip,
          introduced:     Version.new(2,7,0),
          summary:        "Jump forward random generator",
          args:           [[:amount, :number]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Jump the random generator forward essentially skipping the next call to `rand`. You may specify an amount to jump allowing you to skip n calls to `rand`.",
          examples:       ["
  # Basic rand stream skip

  puts rand # prints 0.75006103515625

  rand_skip # jump random stream forward one
            # typically the next rand is 0.733917236328125

  puts rand # prints 0.464202880859375",
  "
  # Jumping forward multiple places in the rand stream

  puts rand # prints 0.75006103515625
  puts rand # prints 0.733917236328125
  puts rand # prints 0.464202880859375
  puts rand # prints 0.24249267578125

  rand_reset  # reset the random stream

  puts rand # prints 0.75006103515625

  rand_skip(2) # jump random stream forward three places
               # the result of the next call to rand will be
               # exactly the same as if rand had been called
               # three times

  puts rand 0.24249267578125"]




      def rand_reset
        SonicPi::Core::SPRand.set_idx!(0)
      end
      doc name:           :rand_reset,
          introduced:     Version.new(2,7,0),
          summary:        "Reset rand generator to last seed",
          args:           [[]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Resets the random stream to the last specified seed. See `use_random_seed` for changing the seed.",
          examples:       ["
  puts rand # prints 0.75006103515625
  puts rand # prints 0.733917236328125
  puts rand # prints 0.464202880859375
  puts rand # prints 0.24249267578125
  rand_reset  # reset the random stream
  puts rand # prints 0.75006103515625
  "]




      def shuffle(list)
        return list.shuffle if list.respond_to? :shuffle
        list.to_a.shuffle
      end
      doc name:           :shuffle,
          introduced:     Version.new(2,1,0),
          summary:        "Randomise order of a list",
          args:           [[:list, :array]],
          opts:           nil,
          accepts_block:  false,
          doc:            "Returns a new list with the same elements as the original but with their order shuffled. Also works for strings",
          examples:       [
        "
  shuffle [1, 2, 3, 4] #=> Would return something like: [3, 4, 2, 1] ",
        "
  shuffle \"foobar\"  #=> Would return something like: \"roobfa\""    ]




      def use_random_seed(seed, &block)
        raise ArgumentError, "use_random_seed does not work with a block. Perhaps you meant with_random_seed" if block
        __thread_locals.set :sonic_pi_spider_new_thread_random_gen_idx, 0

        SonicPi::Core::SPRand.set_seed! seed
      end
      doc name:          :use_random_seed,
          introduced:    Version.new(2,0,0),
          summary:       "Set random seed generator to known seed",
          doc:           "Resets the random number generator to the specified seed. All subsequently generated random numbers and randomisation functions such as `shuffle` and `choose` will use this new generator and the current generator is discarded. Use this to change the sequence of random numbers in your piece in a way that can be reproduced. Especially useful if combined with iteration. See examples.",
          args:          [[:seed, :number]],
          opts:          nil,
          accepts_block: false,
          examples:      ["
  ## Basic usage

  use_random_seed 1 # reset random seed to 1
  puts rand # => 0.417022004702574
  use_random_seed 1 # reset random seed back to 1
  puts rand  #=> 0.417022004702574
  ",
  "
  ## Generating melodies
  notes = (scale :eb3, :minor_pentatonic)  # Create a set of notes to choose from.
                                           # Scales work well for this

  with_fx :reverb do
    live_loop :repeating_melody do         # Create a live loop

      use_random_seed 300                  # Set the random seed to a known value every
                                           # time around the loop. This seed is the key
                                           # to our melody. Try changing the number to
                                           # something else. Different numbers produce
                                           # different melodies

      8.times do                           # Now iterate a number of times. The size of
                                           # the iteration will be the length of the
                                           # repeating melody.

        play notes.choose, release: 0.1    # 'Randomly' choose a note from our ring of
                                           # notes. See how this isn't actually random
                                           # but uses a reproducible method! These notes
                                           # are therefore repeated over and over...
        sleep 0.125
      end
    end
  end
  "  ]




      def with_random_seed(seed, &block)
        raise ArgumentError, "with_random_seed requires a block. Perhaps you meant use_random_seed" unless block
        new_thread_gen_idx = __thread_locals.get :sonic_pi_spider_new_thread_random_gen_idx

        current_seed, current_idx = SonicPi::Core::SPRand.get_seed_and_idx
        SonicPi::Core::SPRand.set_seed! seed
        __thread_locals.set :sonic_pi_spider_new_thread_random_gen_idx, 0
        res = block.call
        SonicPi::Core::SPRand.set_seed! current_seed, current_idx
        __thread_locals.set :sonic_pi_spider_new_thread_random_gen_idx, new_thread_gen_idx
        res
      end
      doc name:           :with_random_seed,
          introduced:     Version.new(2,0,0),
          summary:        "Specify random seed for code block",
          doc:            "Resets the random number generator to the specified seed for the specified code block. All generated random numbers and randomisation functions such as `shuffle` and `choose` within the code block will use this new generator. Once the code block has completed, the original generator is restored and the code block generator is discarded. Use this to change the sequence of random numbers in your piece in a way that can be reproduced. Especially useful if combined with iteration. See examples.",
          args:           [[:seed, :number]],
          opts:           nil,
          accepts_block:  true,
          requires_block: true,
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
            # notice how the original generator is restored",
  "
  ## Generating melodies
  notes = (scale :eb3, :minor_pentatonic, num_octaves: 2)  # Create a set of notes to choose from.
                                           # Scales work well for this

  with_fx :reverb do
    live_loop :repeating_melody do         # Create a live loop

      with_random_seed 300 do              # Set the random seed to a known value every
                                           # time around the loop. This seed is the key
                                           # to our melody. Try changing the number to
                                           # something else. Different numbers produce
                                           # different melodies

        8.times do                         # Now iterate a number of times. The size of
                                           # the iteration will be the length of the
                                           # repeating melody.

          play notes.choose, release: 0.1  # 'Randomly' choose a note from our ring of
                                           # notes. See how this isn't actually random
                                           # but uses a reproducible method! These notes
                                           # are therefore repeated over and over...
          sleep 0.125
        end
      end

      play notes.choose, amp: 1.5, release: 0.5 # Note that this line is outside of
                                                # the with_random_seed block and therefore
                                                # the randomisation never gets reset and this
                                                # part of the melody never repeats.
    end
  end
  "
      ]




      # Give a deprecation warning to users coming from v1.0
      def with_tempo(*args, &block)
        raise DeprecationError, "The function with_tempo is deprecated since v2.0. Please consider use_bpm or with_bpm."
      end


      def use_cue_logging(v, &block)
        raise DeprecationError, "use_cue_logging does not work with a do/end block. Perhaps you meant with_cue_logging" if block
        __thread_locals.set(:sonic_pi_suppress_cue_logging, !v)
      end
      doc name:          :use_cue_logging,
          introduced:    Version.new(2,6,0),
          summary:       "Enable and disable cue logging",
          doc:           "Enable or disable log messages created on cues. This does not disable the cues themselves, it just stops them from being printed to the log",
          args:          [[:true_or_false, :boolean]],
          opts:          nil,
          accepts_block: false,
          examples:      ["use_cue_logging true # Turn on cue messages", "use_cue_logging false # Disable cue messages"]




      def with_cue_logging(v, &block)
        raise ArgumentError, "with_cue_logging requires a do/end block. Perhaps you meant use_cue_logging" unless block
        current = __thread_locals.get(:sonic_pi_suppress_cue_logging)
        __thread_locals.set(:sonic_pi_suppress_cue_logging, !v)
        block.call
        __thread_locals.set(:sonic_pi_suppress_cue_logging, current)
      end
      doc name:          :with_cue_logging,
          introduced:    Version.new(2,6,0),
          summary:       "Block-level enable and disable cue logging",
          doc:           "Similar to use_cue_logging except only applies to code within supplied `do`/`end` block. Previous cue log value is restored after block.",
          args:          [[:true_or_false, :boolean]],
          opts:          nil,
          accepts_block: true,
          requires_block: true,
          examples:      ["
  # Turn on debugging:
  use_cue_logging true

  cue :foo # cue message is printed to log

  with_cue_logging false do
    #Cue logging is now disabled
    cue :bar # cue *is* sent but not displayed in log
  end
  sleep 1
  # Debug is re-enabled
  cue :quux # cue is displayed in log
  "]






      def use_bpm(bpm, &block)
        raise ArgumentError, "use_bpm does not work with a block. Perhaps you meant with_bpm" if block
        raise ArgumentError, "use_bpm's BPM should be a positive value. You tried to use: #{bpm}" unless bpm > 0
        sleep_mul = 60.0 / bpm
        __system_thread_locals.set(:sonic_pi_spider_sleep_mul, sleep_mul)
      end
      doc name:           :use_bpm,
          introduced:     Version.new(2,0,0),
          summary:        "Set the tempo",
          doc:            "Sets the tempo in bpm (beats per minute) for everything afterwards. Affects all subsequent calls to `sleep` and all temporal synth arguments which will be scaled to match the new bpm. If you wish to bypass scaling in calls to sleep, see the fn `rt`. Also, if you wish to bypass time scaling in synth args see `use_arg_bpm_scaling`. See also `with_bpm` for a block scoped version of `use_bpm`.

  For dance music here's a rough guide for which BPM to aim for depending on your genre:

  * Dub: 60-90 bpm
  * Hip-hop: 60-100 bpm
  * Downtempo: 90-120 bpm
  * House: 115-130 bpm
  * Techno/trance: 120-140 bpm
  * Dubstep: 135-145 bpm
  * Drum and bass: 160-180 bpm",
          args:           [[:bpm, :number]],
          opts:           nil,
          accepts_block:  false,
          intro_fn:       true,
          examples:       ["
  # default tempo is 60 bpm
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
    play 62, attack: 0.5, release: 0.25 # attack is scaled to 0.125s and release is now 0.0625s
    sleep 1 # actually sleeps for 0.25 seconds
  end

  "]




      def with_bpm(bpm, &block)
        raise ArgumentError, "with_bpm must be called with a do/end block. Perhaps you meant use_bpm" unless block
        raise ArgumentError, "with_bpm's BPM should be a positive value. You tried to use: #{bpm}" unless bpm > 0
        current_mul = __system_thread_locals.get(:sonic_pi_spider_sleep_mul)
        sleep_mul = 60.0 / bpm
        __system_thread_locals.set(:sonic_pi_spider_sleep_mul, sleep_mul)
        res = block.call
        __system_thread_locals.set(:sonic_pi_spider_sleep_mul, current_mul)
        res
      end
      doc name:           :with_bpm,
          introduced:     Version.new(2,0,0),
          summary:        "Set the tempo for the code block",
          doc:            "Sets the tempo in bpm (beats per minute) for everything in the given block. Affects all containing calls to `sleep` and all temporal synth arguments which will be scaled to match the new bpm. See also `use_bpm`

  For dance music here's a rough guide for which BPM to aim for depending on your genre:

  * Dub: 60-90 bpm
  * Hip-hop: 60-100 bpm
  * Downtempo: 90-120 bpm
  * House: 115-130 bpm
  * Techno/trance: 120-140 bpm
  * Dubstep: 135-145 bpm
  * Drum and bass: 160-180 bpm
  ",
          args:           [[:bpm, :number]],
          opts:           nil,
          accepts_block:  true,
          requires_block: true,
          examples:       ["
  # default tempo is 60 bpm
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




      def with_bpm_mul(mul, &block)
        raise ArgumentError, "with_bpm_mul must be called with a do/end block. Perhaps you meant use_bpm_mul" unless block
        raise ArgumentError, "with_bpm_mul's mul should be a positive value. You tried to use: #{mul}" unless mul > 0
        current_mul = __system_thread_locals.get(:sonic_pi_spider_sleep_mul)
        new_mul = current_mul.to_f / mul
        __system_thread_locals.set(:sonic_pi_spider_sleep_mul, new_mul)
        res = block.call
        __system_thread_locals.set(:sonic_pi_spider_sleep_mul, current_mul)
        res
      end
      doc name:           :with_bpm_mul,
          introduced:     Version.new(2,3,0),
          summary:        "Set new tempo as a multiple of current tempo for block",
          doc:            "Sets the tempo in bpm (beats per minute) for everything in the given block as a multiplication of the current tempo. Affects all containing calls to `sleep` and all temporal synth arguments which will be scaled to match the new bpm. See also `with_bpm`",
          args:           [[:mul, :number]],
          opts:           nil,
          accepts_block:  true,
          requires_block: true,
          examples:       ["
  use_bpm 60   # Set the BPM to 60
  play 50
  sleep 1      # Sleeps for 1 second
  play 62
  sleep 2      # Sleeps for 2 seconds
  with_bpm_mul 0.5 do # BPM is now (60 * 0.5) == 30
    play 50
    sleep 1           # Sleeps for 2 seconds
    play 62
  end
  sleep 1            # BPM is now back to 60, therefore sleep is 1 second
  "]




      def use_bpm_mul(mul, &block)
        raise ArgumentError, "use_bpm_mul must not be called with a block. Perhaps you meant with_bpm_mul" if block
        raise ArgumentError, "use_bpm_mul's mul should be a positive value. You tried to use: #{mul}" unless mul > 0
        current_mul = __system_thread_locals.get(:sonic_pi_spider_sleep_mul)
        new_mul = current_mul.to_f / mul
        __system_thread_locals.set(:sonic_pi_spider_sleep_mul, new_mul)
      end
      doc name:           :use_bpm_mul,
          introduced:     Version.new(2,3,0),
          summary:        "Set new tempo as a multiple of current tempo",
          doc:            "Sets the tempo in bpm (beats per minute) as a multiplication of the current tempo. Affects all containing calls to `sleep` and all temporal synth arguments which will be scaled to match the new bpm. See also `use_bpm`",
          args:           [[:mul, :number]],
          opts:           nil,
          accepts_block:  false,
          examples:       ["
  use_bpm 60   # Set the BPM to 60
  play 50
  sleep 1      # Sleeps for 1 seconds
  play 62
  sleep 2      # Sleeps for 2 seconds
  use_bpm_mul 0.5 # BPM is now (60 * 0.5) == 30
  play 50
  sleep 1           # Sleeps for 2 seconds
  play 62
  "]




      def density(d, &block)
        raise ArgumentError, "density must be called with a do/end block." unless block
        raise ArgumentError, "density must be a positive number. Got: #{d.inspect}." unless d.is_a?(Numeric) && d > 0
        reps = d < 1 ? 1.0 : d
        prev_density = __thread_locals.get(:sonic_pi_local_spider_density) || 1.0
        __thread_locals.set_local(:sonic_pi_local_spider_density, prev_density * d)
        with_bpm_mul d do
          if block.arity == 0
            reps.times do
              block.call
            end
          else
            reps.times do |idx|
              block.call idx
            end
          end
        end
        __thread_locals.set_local(:sonic_pi_local_spider_density, prev_density)
      end
      doc name:           :density,
          introduced:     Version.new(2,3,0),
          summary:        "Squash and repeat time",
          doc:            "Runs the block `d` times with the bpm for the block also multiplied by `d`. Great for repeating sections a number of times faster yet keeping within a fixed time. If `d` is less than 1, then time will be stretched accordingly and the block will take longer to complete.",
          args:           [[:d, :density]],
          opts:           nil,
          accepts_block:  true,
          examples:       ["
  use_bpm 60   # Set the BPM to 60

  density 2 do       # BPM for block is now 120
                     # block is called 2.times
    sample :bd_haus # sample is played twice
    sleep 0.5        # sleep is 0.25s
  end",

  "
  density 2 do |idx| # You may also pass a param to the block similar to n.times
    puts idx         # prints out 0, 1
    sleep 0.5        # sleep is 0.25s
  end
  ",
  "
  density 0.5 do          # Specifying a density val of < 1 will stretch out time
                          # A density of 0.5 will double the length of the block's
                          # execution time.
    play 80, release: 1   # plays note 80 with 2s release
    sleep 0.5             # sleep is 1s
  end
  "    ]




      def current_time
        __system_thread_locals.get(:sonic_pi_spider_time)
      end
      doc name:          :current_time,
          introduced:    Version.new(3,0,0),
          summary:       "Get current (logically quantized) time",
          doc:           "Returns the current logical time. This is a 'wall-clock' time which should typically be pretty similar to Time.now but quantised to a nearby sleep point in the thread. May be quite different to Time.now within a time_warp!

Unlike `Time.now`, Multiple calls to `current_time` with no interleaved calls to `sleep` or `sync` will return the same value.",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      ["
  puts current_time # 2017-03-19 23:37:57 +0000",
"
# The difference between current_time and Time.now
# See that Time.now is continuous and current_time is discrete
#
# {run: 19, time: 0.0}
puts \"A\", Time.now.to_f # ├─ \"A\" 1489966042.761211
puts \"B\", __system_thread_locals.get(:sonic_pi_spider_time).to_f # ├─ \"B\" 1489966042.760181
puts \"C\", Time.now.to_f # ├─ \"C\" 1489966042.761235
puts \"D\", __system_thread_locals.get(:sonic_pi_spider_time).to_f # ├─ \"D\" 1489966042.760181
puts \"E\", __system_thread_locals.get(:sonic_pi_spider_time).to_f # └─ \"E\" 1489966042.760181

"]



      def current_random_seed
        SonicPi::Core::SPRand.get_seed_plus_idx
      end
      doc name:          :current_random_seed,
          introduced:    Version.new(2,10,0),
          summary:       "Get current random seed",
          doc:           "Returns the current random seed.

This can be set via the fns `use_random_seed` and `with_random_seed`. It is incremented every time you use the random number generator via fns such as `choose` and `rand`.",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      ["
  puts current_random_seed # Print out the current random seed",
"
## Resetting the seed back to a known place
puts rand               #=>  0.75006103515625
puts rand               #=>  0.733917236328125
a = current_random_seed # Grab the current seed
puts rand               #=> 0.464202880859375
puts rand               #=> 0.24249267578125
use_random_seed a       # Restore the seed
                        # we'll now get the same random values:
puts rand               #=> 0.464202880859375
puts rand               #=> 0.24249267578125
"]


      def current_bpm
        __current_bpm
      end
      doc name:          :current_bpm,
          introduced:    Version.new(2,0,0),
          summary:       "Get current tempo",
          doc:           "Returns the current tempo as a bpm value.

This can be set via the fns `use_bpm`, `with_bpm`, `use_sample_bpm` and `with_sample_bpm`.",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      ["
  puts current_bpm # Print out the current bpm"]




      def current_beat_duration
        __system_thread_locals.get(:sonic_pi_spider_sleep_mul)
      end
      doc name:          :current_beat_duration,
          introduced:    Version.new(2,6,0),
          summary:       "Duration of current beat",
          doc:           "Get the duration of the current beat in seconds. This is the actual length of time which will elapse with `sleep 1`.

Affected by calls to `use_bpm`, `with_bpm`, `use_sample_bpm` and `with_sample_bpm`.",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      ["
  use_bpm 60
  puts current_beat_duration #=> 1

  use_bpm 120
  puts current_beat_duration #=> 0.5"]




      def beat
        __system_thread_locals.get(:sonic_pi_spider_beat)
      end
      doc name:          :beat,
          introduced:    Version.new(2,10,0),
          summary:       "Get current beat",
          doc:           "Returns the beat value for the current thread/live_loop. Beats are advanced only by calls to `sleep` and `sync`. Beats are distinct from virtual time (the value obtained by calling `vt`) in that it has no notion of rate. It is just essentially a counter for sleeps. After a `sync`, the beat is overridden with the beat value from the thread which called `cue`. ",
          args:          [[]],
          opts:          nil,
          accepts_block: false,
          examples:      ["
  use_bpm 120  # The current BPM makes no difference
  puts beat    #=> 0
  sleep 1
  puts beat    #=> 1
  use_bpm 2000
  sleep 2
  puts beat    #=> 3"]




      def rt(t)
        t / __system_thread_locals.get(:sonic_pi_spider_sleep_mul)
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




      def bt(t)
        t * __system_thread_locals.get(:sonic_pi_spider_sleep_mul)
      end
      doc name:          :bt,
          introduced:    Version.new(2,8,0),
          summary:       "Beat time conversion",
          doc:           "Beat time representation. Scales the time to the current BPM. Useful for adding bpm scaling",
          args:          [[:seconds, :number]],
          opts:          nil,
          accepts_block: false,
          examples:      ["
  use_bpm 120  # Set the BPM to be double the default
  puts bt(1) # 0.5
  use_bpm 60   # BPM is now default
  puts bt(1) # 1
  use_bpm 30   # BPM is now half the default
  puts bt(1) # 2
"]

      def set_sched_ahead_time!(sat)
        t = __system_thread_locals.get(:sonic_pi_spider_time)
        b = __system_thread_locals.get(:sonic_pi_spider_beat)
        i = __current_thread_id
        m = current_bpm
        @system_state.set(t, 0, i, 0, b, m, :sched_ahead_time, sat)
        __info "Schedule ahead time set to #{sat}"
      end
      doc name:          :set_sched_ahead_time!,
          introduced:    Version.new(2,0,0),
          summary:       "Set sched ahead time globally",
          doc:           "Specify how many seconds ahead of time the synths should be triggered. This represents the amount of time between pressing 'Run' and hearing audio. A larger time gives the system more room to work with and can reduce performance issues in playing fast sections on slower platforms. However, a larger time also increases latency between modifying code and hearing the result whilst live coding.",
          args:          [[:time, :number]],
          opts:          nil,
          modifies_env: true,
          accepts_block: false,
          examples:      ["set_sched_ahead_time! 1 # Code will now run approximately 1 second ahead of audio."]



      def use_sched_ahead_time t
        __system_thread_locals.set(:sonic_pi_spider_sched_ahead_time, t)
        __system_thread_locals.set(:sonic_pi_spider_real_time_mode, false)
      end
      doc name:          :use_sched_ahead_time,
          introduced:    Version.new(3,0,0),
          summary:       "Set sched ahead time for the current thread",
          doc:           "Specify how many seconds ahead of time the synths should be triggered. This represents the amount of time between pressing 'Run' and hearing audio. A larger time gives the system more room to work with and can reduce performance issues in playing fast sections on slower platforms. However, a larger time also increases latency between modifying code and hearing the result whilst live coding.

See `set_sched_ahead_time!` for a global version of this function. Note, `use_sched_ahead_time` will override any value set with `set_sched_ahead_time!` for the current thread.

See `use_real_time` for a simple way of setting the schedule ahead time to 0.",
          args:          [[:time, :number]],
          opts:          nil,
          modifies_env: true,
          accepts_block: false,
          examples:      ["use_sched_ahead_time 1 # Code will now run approximately 1 second ahead of audio.",
"# Each thread can have its own sched ahead time
live_loop :foo do
  use_sched_ahead_time 1
  play 70                 # Note 70 will be played with 1 second latency
  sleep 1
end

live_loop :foo do
  use_sched_ahead_time 0.5 # Note 70 will be played with 0.5 second latency
  play 82
  sleep 1
end
"
      ]


      def use_real_time
        __system_thread_locals.set(:sonic_pi_spider_sched_ahead_time, 0)
        __system_thread_locals.set(:sonic_pi_spider_real_time_mode, true)
      end
      doc name:          :use_real_time,
          introduced:    Version.new(3,0,0),
          summary:       "Set sched ahead time to 0 for the current thread",
          doc:           "
Set sched ahead time to 0 for the current thread. Shorthand for `use_sched_ahead_time 0`.

See `use_sched_ahead_time` for a version of this function which allows you to set the schedule ahead time to any arbitrary value. Note, `use_real_time` will override any value set with `set_sched_ahead_time!` for the current thread.

",
          args:          [[]],
          opts:          nil,
          modifies_env: true,
          accepts_block: false,
          examples:      ["use_real_time 1 # Code will now run approximately 1 second ahead of audio."]


      def with_real_time(&blk)
        raise ArgumentError, "with_real_time must be called with a do/end block. Perhaps you meant use_real_time" unless blk
        current_sat = __system_thread_locals.get(:sonic_pi_spider_sched_ahead_time)
        current_rtm = __system_thread_locals.get(:sonic_pi_spider_real_time_mode)
        __system_thread_locals.set(:sonic_pi_spider_sched_ahead_time, 0)
        __system_thread_locals.set(:sonic_pi_spider_real_time_mode, true)
        res = blk.call
        __system_thread_locals.set(:sonic_pi_spider_sched_ahead_time, current_sat)
        __system_thread_locals.set(:sonic_pi_spider_real_time_mode, current_rtm)
        res

      end
      doc name:          :with_real_time,
          introduced:    Version.new(3,0,0),
          summary:       "Sets sched ahead time to 0 within the block for the current thread",
          doc:           "

Sets sched ahead time to 0 within the block for the current thread. Shorthand for `with_sched_ahead_time 0`.

See `with_sched_ahead_time` for a version of this function which allows you to set the schedule ahead time to any arbitrary value. Note, `with_real_time` will override any value set with `set_sched_ahead_time!` for the current thread.

",
          args:          [[]],
          opts:          nil,
          modifies_env: true,
          accepts_block: false,
          examples:      ["use_real_time 1 # Code will now run approximately 1 second ahead of audio."]


      def with_sched_ahead_time t, &blk
        raise ArgumentError, "with_sched_ahead_time must be called with a do/end block. Perhaps you meant use_sched_ahead_time" unless blk
        current_sat = __system_thread_locals.get(:sonic_pi_spider_sched_ahead_time)
        __system_thread_locals.set(:sonic_pi_spider_sched_ahead_time, t)
        res = blk.call
        __system_thread_locals.set(:sonic_pi_spider_sched_ahead_time, current_sat)
        res
      end
      doc name:          :with_sched_ahead_time,
          introduced:    Version.new(3,0,0),
          summary:       "Block-level set sched ahead time for the current thread",
          doc:           "Specify how many seconds ahead of time the synths should be triggered for the block. See `use_sched_ahead_time` for further information.

See `set_sched_ahead_time!` for a global version of this function. Note, `with_sched_ahead_time` will override any value set with `set_sched_ahead_time!` for the given block within the current thread.

See `with_real_time` for a simple way of setting the schedule ahead time to 0.",
          args:          [[:time, :number]],
          opts:          nil,
          modifies_env: true,
          accepts_block: false,
          examples:      ["
with_sched_ahead_time 1 do
  play 70  # Sound will happen with a latency of 1
end

play 70  # Sound will happen with the default latency (0.5s)
"
      ]


      def current_sched_ahead_time
        __current_sched_ahead_time
      end
      doc name:          :current_sched_ahead_time,
          introduced:    Version.new(2,0,0),
          summary:       "Get current sched ahead time",
          doc:           "Returns the current schedule ahead time.

This can be set via the fn `set_sched_ahead_time!`.",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      ["
set_sched_ahead_time! 0.5
puts current_sched_ahead_time # Prints 0.5"]

      def __change_time!(new_vt)
        __system_thread_locals.get(:sonic_pi_spider_time_change).synchronize do
          __system_thread_locals.set :sonic_pi_spider_time, new_vt.freeze

          # free any state waiters if we've advance time sufficiently
          unless  __system_thread_locals.get :sonic_pi_spider_in_time_warp
            __system_thread_locals.get(:sonic_pi_spider_state_waiters).delete_if do |w|
              w[:prom].deliver! true if new_vt > w[:vt]
            end
          end
        end
      end

      def sleep(beats)

        __system_thread_locals.set_local(:sonic_pi_spider_time_state_cache, [])
        __system_thread_locals.set_local(:sonic_pi_local_last_sync, nil)

        # Schedule messages
        __schedule_delayed_blocks_and_messages!
        curr_beat = __system_thread_locals.get(:sonic_pi_spider_beat)
        __system_thread_locals.set(:sonic_pi_spider_beat, curr_beat + beats)
        return if beats == 0

        # Grab the current virtual time
        last_vt = __system_thread_locals.get :sonic_pi_spider_time

        in_time_warp = __system_thread_locals.get(:sonic_pi_spider_in_time_warp)

        # Now get on with syncing the rest of the sleep time...

        # Calculate the amount of time to sleep (take into account current bpm setting)
        sleep_time = beats * __system_thread_locals.get(:sonic_pi_spider_sleep_mul)

        # Calculate the new virtual time
        new_vt = last_vt + sleep_time

        sat = current_sched_ahead_time
        __change_time!(new_vt)

        now = Time.now

        if now - (sat + 0.5) > new_vt
          raise TimingError, "Timing Exception: thread got too far behind time"
        elsif (now - sat) > new_vt
          # TODO: Empirical tests to see what effect this priority stuff
          # actually has on typical workloads

          # Hard warning, system is too far behind, expect timing issues.
          p = Thread.current.priority
          p += 10
          p = 100 if p < 100
          p = 150 if p > 150
          Thread.current.priority = p
          __delayed_serious_warning "Timing error: can't keep up..."
        elsif now > new_vt
          # Soft warning, system should work correctly, but is currently behind
          p = Thread.current.priority
          p += 5
          p = 50 if p < 50
          p = 150 if p > 150
          Thread.current.priority = p
          ## TODO: Remove this and replace with a much better silencing system which
          ## is implemented within the __delayed_* fns
          unless __thread_locals.get(:sonic_pi_mod_sound_synth_silent) || in_time_warp
            __delayed_warning "Timing warning: running slightly behind..."
          end
        else
          if in_time_warp
            # Don't sleep if within a time shift
            # However, do make sure the vt hasn't got too far ahead of the real time
             raise TimingError, "Timing Exception: thread got too far ahead of time" if  (new_vt - 17) > now
          else
            Kernel.sleep new_vt - now
          end
        end


        ## reset control deltas now that time has advanced
        __system_thread_locals.set_local :sonic_pi_local_control_deltas, {}
      end
      doc name:           :sleep,
          introduced:     Version.new(2,0,0),
          summary:        "Wait for beat duration",
          doc:            "Wait for a number of beats before triggering the next command. Beats are converted to seconds by scaling to the current bpm setting.",
          args:           [[:beats, :number]],
          opts:           nil,
          accepts_block:  false,
          advances_time:  true,
          examples:       ["
  # Without calls to sleep, all sounds would happen at once:

  play 50  # This is actually a chord with all notes played simultaneously
  play 55
  play 62

  sleep 1  # Create a gap, to allow a moment's pause for reflection...

  play 50  # Let's try the chord again, but this time with sleeps:
  sleep 0.5 # With the sleeps, we turn a chord into an arpeggio
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
          doc:            "Synonym for `sleep` - see `sleep`",
          args:           [[:beats, :number]],
          opts:           nil,
          accepts_block:  false,
          advances_time:  true,
          examples:       []


      def __live_loop_cue(id)
        t = __system_thread_locals.get(:sonic_pi_spider_time)
        p = -100
        d = __system_thread_locals.get(:sonic_pi_spider_thread_delta)
        __system_thread_locals.set_local(:sonic_pi_spider_thread_delta, d + 1)
        cue_path = "/live_loop/#{id}"
        @register_cue_event_lambda.call(t, p, __current_thread_id, d, current_beat, current_bpm, cue_path, [], __current_sched_ahead_time)
      end




      def sync_bpm(*args)
        params, opts = split_params_and_merge_opts_array(args)
        opts[:bpm_sync] = true
        sync(*params, opts)
      end
      doc name:           :sync_bpm,
          introduced:     Version.new(2,10,0),
          summary:        "Sync and inherit BPM from other threads ",
          doc:            "An alias for `sync` with the `bpm_sync:` opt set to true.",
          args:           [[:cue_id, :symbol]],
          opts:           {},
          accepts_block:  false,
          advances_time:  true,
          examples:       ["See examples for sync"]



      def sync_event(*args)
        params, opts = split_params_and_merge_opts_array(args)
        k = params[0]

        __system_thread_locals.set_local(:sonic_pi_spider_time_state_cache, [])
        # TODO: need to add this
        bpm_sync = truthy?(opts[:bpm_sync])
        arg_matcher = opts[:arg_matcher]

        cue_id = __sync_path(k)
        last_sync = __system_thread_locals.get(:sonic_pi_local_last_sync, nil)

        __system_thread_locals.set_local :sonic_pi_local_control_deltas, {}

        unless __thread_locals.get(:sonic_pi_suppress_cue_logging)
          __delayed_highlight3_message "sync #{k.inspect}"
        end

        __schedule_delayed_blocks_and_messages!

        if last_sync
          t = last_sync.time
          i = last_sync.thread_id
          p = last_sync.priority
          d = last_sync.delta
          b = last_sync.beat
          m = last_sync.bpm
        else
          # TODO insert priority and delta values here:
          t = current_time
          p = __system_thread_locals.get(:sonic_pi_spider_thread_priority, -100)
          i = __current_thread_id
          d = __system_thread_locals.get(:sonic_pi_spider_thread_delta, 0)
          b = current_beat
          m = current_bpm
        end

        se = @event_history.sync(t, p, i, d, b, m, cue_id, arg_matcher)

        __system_thread_locals.set(:sonic_pi_spider_synced, true)
        __system_thread_locals.set :sonic_pi_spider_beat, se.beat

        __change_time!(se.time)
        __system_thread_locals.set_local :sonic_pi_local_last_sync, se

        if bpm_sync
          bpm = se.bpm <= 0 ? 60 : se.bpm
          __system_thread_locals.set(:sonic_pi_spider_sleep_mul, 60.0 / bpm)
        end

        run_info = ""

        unless __thread_locals.get(:sonic_pi_suppress_cue_logging)
          if bpm_sync

            __delayed_highlight2_message "synced #{cue_id.inspect}. Inheriting bpm of #{current_bpm} " + run_info
          else
            __delayed_highlight2_message "synced #{cue_id.inspect} " + run_info
          end
        end
        __system_thread_locals.set_local :sonic_pi_local_last_sync, se
        se
      end

      def sync(*args)
        sync_event(*args).val
      end
      doc name:           :sync,
          introduced:     Version.new(2,0,0),
          summary:        "Sync with other threads",
          doc:            "Pause/block the current thread until a `cue` heartbeat with a matching `cue_id` is received. When a matching `cue` message is received, unblock the current thread, and continue execution with the virtual time set to match the thread that sent the `cue` heartbeat. The current thread is therefore synced to the `cue` thread. If multiple cue ids are passed as arguments, it will `sync` on the first matching `cue_id`. The BPM of the cueing thread can optionally be inherited by using the bpm_sync: opt.",
          args:           [[:cue_id, :symbol]],
          opts:           {:bpm_sync => "Inherit the BPM of the cueing thread. Default is false"},
          accepts_block:  false,
          advances_time:  true,
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
      sleep 0.5  # and sleeping for 0.5 beats between ticks
    end
  end

  # We can now play sounds using the metronome.
  loop do                    # In the main thread, just loop
    sync :tick               # waiting for :tick sync messages
    sample :drum_heavy_kick  # after which play the drum kick sample
  end",

  "
  sync :foo, :bar # Wait for either a :foo or :bar cue ",

  "
  in_thread do   # Start a metronome thread
    loop do      # Loop forever:
      cue [:foo, :bar, :baz].choose # sending one of three tick heartbeat messages randomly
      sleep 0.5  # and sleeping for 0.5 beats between ticks
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
        __in_thread(*opts, &block)
      end
      doc name:           :in_thread,
          introduced:     Version.new(2,0,0),
          summary:        "Run code block at the same time",
          doc:            "Execute a given block (between `do` ... `end`) in a new thread. Use for playing multiple 'parts' at once. Each new thread created inherits all the use/with defaults of the parent thread such as the time, current synth, bpm, default synth args, etc. Despite inheriting defaults from the parent thread, any modifications of the defaults in the new thread will *not* affect the parent thread. Threads may be named with the `name:` optional arg. Named threads will print their name in the logging pane when they print their activity. If you attempt to create a new named thread with a name that is already in use by another executing thread, no new thread will be created.

It is possible to delay the initial trigger of the thread on creation with both the `delay:` and `sync:` opts. See their respective docstrings. If both `delay:` and `sync:` are specified, on initial thread creation first the delay will be honoured and then the sync.
",
          args:           [],
          opts:           {:name  => "Make this thread a named thread with name. If a thread with this name already exists, a new thread will not be created.",
                           :delay => "Initial delay in beats before the thread starts. Default is 0.",
                           :sync => "Initial sync symbol. Will sync with this symbol before the thread starts.",
                           :sync_bpm => "Initial sync symbol. Will sync with this symbol before the live_loop starts. Live loop will also inherit the BPM of the thread which cued the symbol.",},
          accepts_block:  true,
          requires_block: true,
          async_block:    true,
          examples:       ["
  loop do      # If you write two loops one after another like this,
    play 50    # then only the first loop will execute as the loop acts
    sleep 1    # like a trap not letting the flow of control out
  end

  loop do      # This code is never executed.
    play 55
    sleep 0.5
  end ",

  "

  # In order to play two loops at the same time, the first loops need to
  # be in a thread (note that it's probably more idiomatic to use live_loop
  # when performing):

  # By wrapping our loop in an in_thread block, we split the
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
    play 38        # Play note 38 at time 0.5
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

  # We are now free to modify the contents of :foo and re-run the entire buffer.
  # We'll hear the effect immediately without having to stop and re-start the code.
  # This is because our fn has been redefined, (which our thread will pick up) and
  # due to the thread being named, the second re-run will not create a new similarly
  # named thread. This is a nice pattern for live coding and is the basis of live_loop.
  ",
  "
  #Delaying the start of a thread
  in_thread delay: 1 do
    sample :ambi_lunar_land # this sample is not triggered at time 0 but after 1 beat
  end

  play 80                   # Note 80 is played at time 0
  "    ]

      def assert_inherited(arg, klass=Object)
        unless arg.is_a?(klass)
          raise AssertionError, "Assert inherited failed! #{arg.inspect} is not a descendent of #{klass}"
        end
        arg
      end

      def assert_error(klass=Exception, &blk)
        raised_exception = nil
        begin
          blk.call
        rescue klass => e
          return nil
        rescue Exception => e
          raised_exception = e.class
        end

        if raised_exception
          raise AssertionError, "Assert error failed! #{klass.inspect} not raised by running do/end block, instead block raised #{raised_exception.inspect}"
        else
          raise AssertionError, "Assert error failed! No errors raised by running do/end block"
        end
      end
      doc name:           :assert_error,
          introduced:     Version.new(3,0,0),
          summary:        "Ensure block throws an error",
          doc:            "Runs the block and ensures that it raises the correct Exception. Useful for asserting that an Exception will be raised. You may specify the particular Exception class, which defaults to `Exception`.",
          args:           [[:class, :Exception]],
          opts:           nil,
          accepts_block:  true,
          examples:       ["
assert_error do
  play 70
end                         # Will throw an exception: \"Assert error failed!\" as the block
                            # contains no errors.
",
      "
assert_error do
  1 / 0
end                         # Will not throw an exception as the block contains an error.",

        "
assert_error ZeroDivisionError do
  1 / 0
end                         # Will not throw an exception as the block contains a ZeroDivisionError.",
        "
assert_error ThreadError do
  1 / 0
end                         # Will throw an exception as the block contains a ZeroDivisionError rather than
                            # a ThreadError."]



      def assert(arg, msg=nil)
        unless arg
          error_msg =  "Assert failed! #{msg}"
          raise AssertionError, error_msg
        end

        arg
      end
      doc name:           :assert,
          introduced:     Version.new(2,8,0),
          summary:        "Ensure arg is valid",
          doc:            "Raises an exception if the argument is either nil or false.",
          args:           [[:arg, :anything]],
          alt_args:       [[:arg, :anything],[:error_msg, :string]],
          opts:           nil,
          accepts_block:  false,
          examples:       ["
# Simple assertions
assert true   # As true is neither nil or false, this assertion passes
assert 1      # Similarly, 1 passes
assert \"foo\" # As do string
assert false  # This will raise an exception
",
"
# Communicating error messages
assert false, \"oops\" # This will raise an exception containing the message \"oops\"
",

"
# More interesting assertions
assert (1 + 1) == 2 # Ensure that arithmetic is sane!
assert [:a, :b, :c].size == 3 # ensure lists can be correctly counted
"]

      def assert_equal(arg1, arg2, msg=nil)
        unless arg1 == arg2
          error_msg =  "Assert failed! #{arg1.inspect} is not equal to #{arg2.inspect}. #{msg}"
          raise AssertionError, error_msg
        end
        arg1
      end
      doc name:           :assert_equal,
          introduced:     Version.new(2,8,0),
          summary:        "Ensure args are equal",
          doc:            "Raises an exception if both arguments aren't equal. ",
          args:           [[:arg1, :anything], [:arg2, :anything]],
          alt_args:       [[:arg1, :anything], [:arg2, :anything],[:error_msg, :string]],
          opts:           nil,
          accepts_block:  false,
          examples:       ["
# Simple assertions
assert_equal 1, 1
",
"
# More interesting assertions
assert_equal 1 + 1, 2 # Ensure that arithmetic is sane!
assert_equal [:a, :b, :c].size,  3 # ensure lists can be correctly counted
",

"
# Add messages to the exceptions
assert_equal 3, 5, \"something is seriously wrong!\"
" ]

      def assert_similar(a, b, msg=nil)
        case a
          when Numeric
          assert_equal(a.to_f.round(8), b.to_f.round(8), msg)
        else
          assert_equal(a, b, msg)
        end
      end
      doc name:           :assert_similar,
          introduced:     Version.new(3,0,0),
          summary:        "Ensure args are similar",
          doc:            "Raises an exception if both arguments aren't similar.

Currently similarity is only defined for numbers - all other types are compared for equality with assert_equal.

Useful for testing in cases where floating point imprecision stops you from being able to use `assert_equal`. ",
          args:           [[:arg1, :anything], [:arg2, :anything]],
          alt_args:       [[:arg1, :anything], [:arg2, :anything],[:error_msg, :string]],
          opts:           nil,
          accepts_block:  false,
          examples:       ["
# Simple assertions
assert_similar 1, 1 #=> True
",
"
# Handles floating point imprecision
assert_similar(4.9999999999, 5.0) #=> True"
      ]

      def load_buffer(path)
        path = File.expand_path(path.to_s)
        raise IOError, "Unable to load buffer - no file found with path: #{path}" unless File.exist?(path)
        buf = __current_job_info[:workspace]
        __info "loading #{buf} with #{path}"
        __replace_buffer(buf, File.read(path))
      end
      doc name:           :load_buffer,
          introduced:     Version.new(2,10,0),
          summary:        "Load the contents of a file to the current buffer",
          doc:            "Given a path to a file, will read the contents and load it into the current buffer. This will replace any previous content.",
          args:           [[:path, :string]],
          opts:           nil,
          accepts_block:  false,
          examples:       ["
load_buffer \"~/sonic-pi-tracks/phat-beats.rb\" # will replace content of current buffer with contents of the file"]


      def load_example(example_name)
        path = Dir[examples_path + '/**/' + example_name.to_s + '.rb'].first
        raise IOError, "Error - no example found with name: #{example_name.inspect}" unless path
        buf = __current_job_info[:workspace]
        __info "loading #{buf} with #{path}"
        title = ActiveSupport::Inflector.titleize(example_name)
        __replace_buffer(buf, "# #{title}\n" + File.read(path))
      end
      doc name:           :load_example,
          introduced:     Version.new(2,10,0),
          summary:        "Load a built-in example",
          doc:            "Given a keyword representing an example, will load it into the current buffer. This will replace any previous content.",
          args:           [[:path, :string]],
          opts:           nil,
          accepts_block:  false,
          examples:       ["
load_example :rerezzed # will replace content of current buffer with the rerezzed example"]

      def __on_thread_death(&block)
        gc_jobs = __system_thread_locals.get(:sonic_pi_local_spider_in_thread_gc_jobs) || []
        gc_jobs << block
        __system_thread_locals.set_local(:sonic_pi_local_spider_in_thread_gc_jobs, gc_jobs)
      end
    end
  end
end
