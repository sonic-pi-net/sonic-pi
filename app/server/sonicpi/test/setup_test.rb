require_relative "../../core"
require_relative "../lib/sonicpi/runtime"
require 'minitest/autorun'

module SonicPi


  class MockStudio
    def sched_ahead_time
      0.5
    end
  end

  class MockLang
    attr_accessor :mod_sound_studio, :sample_loader, :msg_queue
    include SonicPi::RuntimeMethods
    include SonicPi::Lang::Core
    include SonicPi::Lang::Sound

    def initialize
      @job_subthread_mutex = Mutex.new
      @job_counter = Object.new
      @job_counter.stubs(:next).returns 1
      @mod_sound_studio = MockStudio.new

      @msg_queue = Queue.new

      __set_default_user_thread_locals!
      now = Time.now.freeze
      __system_thread_locals.set :sonic_pi_spider_time, now
      __system_thread_locals.set :sonic_pi_spider_start_time, now
      __system_thread_locals.set :sonic_pi_spider_beat, 0
      __system_thread_locals.set :sonic_pi_spider_job_id, 1
      __system_thread_locals.set_local :sonic_pi_local_spider_delayed_messages, []
    end

    def __schedule_delayed_blocks_and_messages!(*args)
      # do nothing
    end
  end



end
