require_relative "../../core"
require_relative "../lib/sonicpi/runtime"
require_relative "../lib/sonicpi/lang/core"
require_relative "../lib/sonicpi/event_history"
require_relative "../lib/sonicpi/thread_id"
require 'minitest'
require 'minitest/autorun'

module SonicPi
  class MockStudio
  end

  class MockLang
    attr_accessor :mod_sound_studio, :sample_loader, :msg_queue
    include SonicPi::RuntimeMethods
    include SonicPi::Lang::Core
    include SonicPi::Lang::Sound

    include ActiveSupport
    def initialize
      @mod_sound_studio = MockStudio.new

      @msg_queue = Queue.new
      __set_default_user_thread_locals!

      @system_state = EventHistory.new
      @user_state = EventHistory.new
      @osc_state = EventHistory.new
      @system_init_thread_id = ThreadId.new(-1)
      @system_state.set 0, 0, @system_init_thread_id, 0, 0, :sched_ahead_time, 0.5

      @settings = Config::Settings.new("/bogus/path/to/default/to/empty/settings.txt")
      @version = Version.new(0, 0, 0, "test")
      @server_version = Version.new(1, 0, 0, "final")
      @life_hooks = LifeCycleHooks.new
      @msg_queue = Queue.new
      @event_queue = SizedQueue.new(20)
      @keypress_handlers = {}
      @cue_events = IncomingEvents.new
      @sync_counter = Counter.new
      @job_counter = Counter.new(-1) # Start counting jobs from 0
      @job_subthreads = {}
      @job_main_threads = {}
      @named_subthreads = {}
      @job_subthread_mutex = Mutex.new
      @user_jobs = Jobs.new
      @sync_real_sleep_time = 0.05
      @user_methods = []
      @global_start_time = Time.now
      @session_id = SecureRandom.uuid
      @snippets = {}
      @gui_cue_log_idxs = Counter.new
    end

    def run(&blk)
      t = Thread.new do

        id = 0
        silent = false
        info = {}.freeze
        now = Time.now.freeze

        reg_job 0, Thread.current
        __system_thread_locals.set_local :sonic_pi_local_thread_group, "job-#{id}"
        __system_thread_locals.set_local :sonic_pi_spider_thread_id_path, ThreadId.new(id)
        __system_thread_locals.set :sonic_pi_spider_job_id, id
        __system_thread_locals.set :sonic_pi_spider_silent, silent
        __system_thread_locals.set :sonic_pi_spider_job_info, info

        __system_thread_locals.set :sonic_pi_spider_time, now
        __system_thread_locals.set :sonic_pi_spider_start_time, now
        __system_thread_locals.set :sonic_pi_spider_beat, 0
        __system_thread_locals.set_local :sonic_pi_local_spider_delayed_messages, []

        __set_default_system_thread_locals!
        __set_default_user_thread_locals!

        t2 = in_thread do
          clear
          self.instance_eval(&blk)
        end

        t2.join
      end
      t.join
    end

    def __schedule_delayed_blocks_and_messages!(*args)
      # do nothing
    end
  end



end
