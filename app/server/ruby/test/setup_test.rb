require_relative "../core"
require_relative "../lib/sonicpi/runtime"
require_relative "../lib/sonicpi/lang/core"
require_relative "../lib/sonicpi/lang/midi"
require_relative "../lib/sonicpi/event_history"
require_relative "../lib/sonicpi/thread_id"
require_relative "../lib/sonicpi/lang/western_theory"
require 'minitest'
# Use only the vendored minitest. Minitest's at-exit plugin discovery
# (Gem.find_files "minitest/*_plugin.rb") otherwise loads the system minitest
# gem on top of the vendored copy; the double-load breaks test discovery so
# every file reports "0 runs". Disabling the cross-gem plugin scan keeps a
# single minitest loaded.
def (Minitest).load_plugins; end
require 'minitest/autorun'

# Mocha renamed its Minitest integration entry point in 1.5. The vendored copy
# predates the rename; distribution builds run against a system gem that has
# since dropped the old name, so accept whichever is present. The two must not
# interleave: the vendored lib dir sits first on the load path, so a system
# mocha would resolve its internal requires against the vendored 1.1.0 files.
# Take the vendored copy off the path unless it is the one being used.
vendored_mocha = $LOAD_PATH.find { |p| p.include?("vendor/mocha") }
$LOAD_PATH.delete(vendored_mocha)
begin
  require 'mocha/minitest'
rescue LoadError
  $LOAD_PATH.unshift(vendored_mocha) if vendored_mocha
  require 'mocha/setup'
end

module SonicPi
  class MockStudio

  end

  class MockLinkAPI
    def link_tempo(*); 60.0; end
    def link_is_playing?(*); false; end
    def link_transport_state(*); { playing: false, anchored: false }; end
    def link_set_bpm!(*); end
    def link_sleep(*); end
    def link_get_beat_at_clock_time(*); 0.0; end
    def link_get_clock_time_at_beat(*); Time.now.to_f; end
    def link_get_next_beat_and_clock_time_at_phase(*); [0.0, Time.now.to_f]; end
    def link_audio_input_set!(*); end
    def link_audio_input_remove!(*); end
    def link_audio_inputs_clear!(*); end
  end

  class MockOscAPI
    def send_osc_at(*); end
    def osc_flush!(*); end
    def start_stop_cue_server!(*); end
    def cue_server_internal!(*); end
    def set_global_timewarp!(*); end
  end

  class MockMidiAPI
    def midi_send_at(*); end
    def midi_flush!(*); end
    def midi_system_start!(*); end
    def midi_system_stop!(*); end
    def set_global_timewarp!(*); end
  end

  class MockLang
    attr_accessor :mod_sound_studio, :sample_loader, :msg_queue, :event_history
    include SonicPi::RuntimeMethods
    include SonicPi::Lang::Core
    include SonicPi::Lang::WesternTheory
    include SonicPi::Lang::Sound
    include SonicPi::Lang::Midi

    def initialize
      @mod_sound_studio = MockStudio.new
      @msg_queue = Queue.new
      __set_default_user_thread_locals!
      @system_init_thread_id = ThreadId.new(-1)

      @settings = Config::Settings.new("/bogus/path/to/default/to/empty/settings.txt")
      @version = Version.new(0, 0, 0, "test")
      @server_version = Version.new(1, 0, 0, "final")


      @life_hooks = LifeCycleHooks.new
      @cue_events = IncomingEvents.new
      @job_counter = Counter.new(-1) # Start counting jobs from 0
      @job_subthreads = {}
      @job_main_threads = {}
      @named_subthreads = {}
      @job_subthread_mutex = Mutex.new
      @osc_cue_server_mutex = Mutex.new
      @user_jobs = Jobs.new
      @session_id = SecureRandom.uuid
      @snippets = {}
      @system_state = EventHistory.new
      @user_state = EventHistory.new
      @event_history = EventHistory.new
      @system_init_thread_id = ThreadId.new(-1)
      @gui_cue_log_idxs = Counter.new
      @gui_heartbeats = {}
      @gui_last_heartbeat = nil

      @register_cue_event_lambda = lambda do |t, p, i, d, b, m, address, args, sched_ahead_time=0|
        t = t.to_r
        sym = nil
        address, sym = *address if address.is_a?(Array)

        gui_log_id = @gui_cue_log_idxs.next
        args = args.__sp_make_thread_safe
        address = address.to_s.freeze
        @event_history.set(t, p, i, d, b, m, address, args)
        @cue_events.async_event("/spider_thread_sync/#{address}", {
                                  :time => t,
                                  :cue_splat_map_or_arr => args,
                                  :cue => address })

        sched_ahead_sync_t = t + sched_ahead_time

        sleep_time = sched_ahead_sync_t - Time.now.to_r
        if sleep_time > 0
          Thread.new do
            Kernel.sleep(sleep_time)
            __msg_queue.push({:type => :incoming, :time => t.to_s, :id => gui_log_id, :address => address, :args => args.inspect})
            __msg_queue.push({:type => :incoming, :time => t.to_s, :id => gui_log_id, :address => sym, :args => args.inspect}) if sym
          end
        else
          __msg_queue.push({:type => :incoming, :time => t.to_s, :id => gui_log_id, :address => address, :args => args.inspect})
          __msg_queue.push({:type => :incoming, :time => t.to_s, :id => gui_log_id, :address => sym, :args => args.inspect}) if sym
        end
      end

      # external_osc_cue_handler = lambda do |time, ip, port, address, args|
      #   address = "/#{address}" unless address.start_with?("/")
      #   address = "/osc:#{host}:#{port}#{address}"
      #   p = 0
      #   d = 0
      #   b = 0
      #   m = 60
      #   @register_cue_event_lambda.call(Time.now, p, @system_init_thread_id, d, b, m, address, args, 0)
      # end

      # internal_cue_handler = lambda do |path, args|
      #   p = 0
      #   d = 0
      #   b = 0
      #   m = 60
      #   @register_cue_event_lambda.call(Time.now, p, @system_init_thread_id, d, b, m, address, args, 0)
      # end

      # updated_midi_ins_handler = lambda do |ins|
      #   desc = ins.join("\n")
      #   __msg_queue.push({:type => :midi_in_ports, :val => desc})
      # end

      # updated_midi_outs_handler = lambda do |outs|
      #   desc = outs.join("\n")
      #   __msg_queue.push({:type => :midi_out_ports, :val => desc})
      # end

      # @tau_api = TauAPI.new(ports,
      #                       {
      #                         external_osc_cue: external_osc_cue_handler,
      #                         internal_cue: internal_cue_handler,
      #                         updated_midi_ins: updated_midi_ins_handler,
      #                         updated_midi_outs: updated_midi_outs_handler
      #                       })

      @link_api = MockLinkAPI.new
      @osc_api = MockOscAPI.new
      @midi_api = MockMidiAPI.new

      begin
        @gitsave = GitSave.new(Paths.project_path)
      rescue
        @gitsave = nil
      end

      @save_queue = SizedQueue.new(20)

      @save_t = Thread.new do
        __system_thread_locals.set_local(:sonic_pi_local_thread_group, :save_loop)
        Kernel.loop do
          event = @save_queue.pop
          id, content = *event
          filename = id + '.spi'
          path = File.expand_path("#{Paths.project_path}/#{filename}")
          content = filter_for_save(content)
          begin
            File.open(path, 'w') {|f| f.write(content) }
            @gitsave.save!(filename, content, "#{@version} -- #{@session_id} -- ")
          rescue Exception => e
            log "Exception saving buffer #{filename}:\n#{e.inspect}"
            ##TODO: remove this and ensure that git saving actually works
            ##instead of cowardly hiding the issue!
          end
        end
      end

      @system_state.set 0, 0, ThreadId.new(-2), 0, 0, 60, :sched_ahead_time, default_sched_ahead_time

      __info "Welcome to Sonic Pi #{version}", 1

      __info "Running on Ruby v#{RUBY_VERSION}"

      if safe_mode?
        __info "!!WARNING!! - file permissions issue:\n   Unable to write to folder #{Paths.home_dir_path} \n   Booting in SAFE MODE.\n   Buffer auto-saving is disabled, please save your work manually.", 1
      end

      log "Unable to initialise git repo at #{Paths.project_path}" unless @gitsave
      load_snippets(Paths.snippets_path, true)
    end


    def __error(e, m=nil)
      raise e
    end

    def run(&blk)
      id            = 0
      silent        = false
      info          = {}.freeze
      now           = Time.now.freeze
      job_in_thread = nil
      job           = Thread.new do
        Thread.current.abort_on_exception = true

        register_job! 0, Thread.current
        __system_thread_locals.set_local :sonic_pi_local_thread_group, "job-#{id}"
        __system_thread_locals.set_local :sonic_pi_spider_thread_id_path, ThreadId.new(id)
        __system_thread_locals.set :sonic_pi_spider_job_id, id
        __system_thread_locals.set :sonic_pi_spider_silent, silent
        __system_thread_locals.set :sonic_pi_spider_job_info, info
        __init_spider_time_and_beat!
        __system_thread_locals.set_local :sonic_pi_local_spider_delayed_messages, []

        __set_default_system_thread_locals!
        __set_default_user_thread_locals!

        job_in_thread = in_thread do
          clear
          self.instance_eval(&blk)
        end
      end

      @user_jobs.add_job(id, job, info)

      t = Thread.new do
        Thread.current.priority = -10
        __system_thread_locals.set_local(:sonic_pi_local_thread_group, "job-#{id}-GC")
        job.join
        __system_thread_locals(job_in_thread).get(:sonic_pi_local_spider_subthread_empty).get
        # wait until all synths are dead
        @life_hooks.completed(id)
        @life_hooks.exit(id, {:start_t => now})
        deregister_job_and_return_subthreads(id)
        @user_jobs.job_completed(id)
        Kernel.sleep default_sched_ahead_time
        unless @user_jobs.any_jobs_running?
          @life_hooks.all_completed(silent)
        end
      end

      t.join
    end

    def __schedule_delayed_blocks_and_messages!(*args)
      # do nothing
    end

  end



end
