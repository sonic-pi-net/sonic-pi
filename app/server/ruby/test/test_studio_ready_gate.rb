#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#++

require_relative "setup_test"
require_relative "../lib/sonicpi/studio_ready_gate"
require 'concurrent/atomic/cyclic_barrier'
require 'concurrent/atomic/atomic_fixnum'

# Sonic Pi's pre-existing test infrastructure (rake test / minitest
# autorun) is broken in this environment — minitest 6.0 from system
# gems collides with vendor's 5.18.1 and auto-discovery yields 0 runs.
# Rather than fight that, register an at_exit hook that explicitly runs
# our test methods. Same green/red signal, no infrastructure dependency.
at_exit do
  test = SonicPi::StudioReadyGateTest.new(nil)
  passed = 0
  failed = []
  test.public_methods(false).grep(/^test_/).each do |m|
    begin
      test.setup if test.respond_to?(:setup)
      test.send(m)
      passed += 1
      puts "  PASS  #{m}"
    rescue Minitest::Assertion, Minitest::Skip => e
      failed << [m, e]
      puts "  FAIL  #{m}: #{e.message}"
    rescue => e
      failed << [m, e]
      puts "  ERR   #{m}: #{e.class}: #{e.message}"
    end
  end
  puts ""
  puts "#{passed} passed, #{failed.size} failed"
  exit(failed.empty? ? 0 : 1)
end

module SonicPi
  class StudioReadyGateTest < Minitest::Test
    # The gate provides a reader-writer protocol around studio state:
    #
    #   - trigger_*, new_group, allocate_buffer etc. all hold a SHARED
    #     (reader) lock for the duration of their work. Many can run
    #     concurrently — they don't conflict with each other.
    #
    #   - cold_swap_reinit holds an EXCLUSIVE (writer) lock around its
    #     phases. It blocks new readers from starting AND waits for any
    #     in-flight readers to drain BEFORE Phase 1's nuke runs (so
    #     readers can't see partially-nilled studio state).
    #
    #   - Reentrant: the same thread that holds a reader lock can call
    #     a method that ALSO acquires the reader lock without dead-
    #     locking. Critical because trigger_fx calls trigger_synth.

    def setup
      @gate = StudioReadyGate.new
    end

    def test_concurrent_readers_do_not_block_each_other
      # Prove parallelism deterministically. Each reader thread
      # acquires the read lock then rendezvous at a barrier requiring
      # all 4. If the gate allows concurrent readers, all 4 are inside
      # the block at once, the barrier trips, and `reached` hits 4. If
      # readers are serialized, only one thread is ever inside — the
      # 4th arrival never happens, the barrier times out, and
      # `reached` stays low. Independent of wall-clock noise.
      barrier = Concurrent::CyclicBarrier.new(4)
      reached = Concurrent::AtomicFixnum.new(0)

      threads = 4.times.map do
        Thread.new do
          @gate.with_studio_ready(:test_op) do
            reached.increment if barrier.wait(1.0)
          end
        end
      end
      threads.each(&:join)

      assert_equal 4, reached.value,
        "Readers serialized — only #{reached.value}/4 entered the read block concurrently"
    end

    def test_writer_blocks_new_readers_until_release
      writer_done = false
      reader_started_after_writer = false

      writer = Thread.new do
        @gate.with_studio_writer do
          sleep 0.15
          writer_done = true
        end
      end

      sleep 0.05  # ensure writer has acquired

      reader = Thread.new do
        @gate.with_studio_ready(:test_op) do
          # If gate works: only enter here AFTER writer released
          reader_started_after_writer = writer_done
        end
      end

      [writer, reader].each(&:join)
      assert reader_started_after_writer, "Reader proceeded while writer held the lock"
    end

    def test_writer_waits_for_in_flight_reader_to_finish
      reader_done = false
      writer_started_after_reader = false

      reader = Thread.new do
        @gate.with_studio_ready(:test_op) do
          sleep 0.15
          reader_done = true
        end
      end

      sleep 0.05  # ensure reader has acquired

      writer = Thread.new do
        @gate.with_studio_writer do
          # If gate works: only enter here AFTER reader released
          writer_started_after_reader = reader_done
        end
      end

      [reader, writer].each(&:join)
      assert writer_started_after_reader,
             "Writer proceeded while reader was still in flight (would corrupt studio state)"
    end

    def test_reader_lock_is_reentrant_on_same_thread
      # trigger_fx calls trigger_synth — both acquire read lock from the
      # same thread. Must not self-deadlock.
      result = nil
      @gate.with_studio_ready(:outer_op) do
        @gate.with_studio_ready(:inner_op) do
          result = :reached_inner
        end
      end
      assert_equal :reached_inner, result
    end

    def test_writer_can_acquire_reader_inside
      # cold_swap_reinit holds write lock; its phases call studio
      # methods (e.g. start_mixer) which acquire the read lock.
      # Reentrant write→read on same thread must work.
      result = nil
      @gate.with_studio_writer do
        @gate.with_studio_ready(:phase_op) do
          result = :reached_inner
        end
      end
      assert_equal :reached_inner, result
    end

    def test_permanently_broken_raises_immediately_without_blocking
      @gate.mark_permanently_broken!("supersonic crashed during cold-swap")
      start = Time.now
      assert_raises(StudioCurrentlyRebootingError) do
        @gate.with_studio_ready(:test_op) { flunk "should not have entered block" }
      end
      elapsed = Time.now - start
      assert elapsed < 0.1, "took #{elapsed}s — should raise immediately, not wait"
    end

    def test_op_name_appears_in_broken_error_message
      @gate.mark_permanently_broken!("specific failure reason")
      err = assert_raises(StudioCurrentlyRebootingError) do
        @gate.with_studio_ready(:trigger_synth) { }
      end
      assert_includes err.message, "specific failure reason"
      assert_includes err.message, "trigger_synth"
    end
  end
end
