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

require_relative "../../setup_test"
require_relative "../../../lib/sonicpi/lang/core"


module SonicPi
  class CueSyncTester < Minitest::Test

    def setup
      @lang = SonicPi::MockLang.new
    end


    def test_time_increment
      @lang.run do

        assert_equal 0, vt
        sleep 0.05
        assert_equal 0.05, vt

        in_thread do
          sleep 0.1
          cue :foo
        end

        sync :foo

        assert_equal 0.15,  vt
        sleep 0.1
        assert_equal 0.25,  vt

        in_thread do
          sleep 0.1
          cue :foo
        end

        sync :foo

        assert_equal 0.35,  vt

        in_thread do
          sleep 0.02
          cue :foo
        end

        sync :foo

        assert_equal 0.37,  vt
      end
    end

    # syncing should return the 'next' event from the current
    # time.

    def test_time_increment_w_faster_bpm
      @lang.run do
        use_bpm 120
        assert_equal 0, vt
        sleep 0.1
        assert_equal 0.1 / 2, vt

        in_thread do
          sleep 0.05
          cue :foo
        end

        sync :foo

        assert_equal 0.15 / 2,  vt
        sleep 0.02
        assert_equal 0.17 / 2,  vt
        in_thread do
          sleep 0.04
          cue :foo
        end

        sync :foo

        assert_equal 0.21 / 2,  vt

        in_thread do
          sleep 0.05
          cue :foo, 2
        end

        in_thread do
          sleep 0.05
          cue :foo, 3
        end

        v = sync :foo

        assert_equal 0.26 / 2, vt
        assert_equal 2, v[0]
      end
    end

    def test_loose_thread_order
      @lang.run do
        assert_equal 0, vt

        # Sync in early thread
        in_thread do
          assert_equal 0, vt
          sync :foo
          assert_equal 0, vt
        end

        in_thread do
          # Cue in the middle thread
          cue :foo
          sleep 0.1
          cue :foo
        end

        # Sync in late thread
        in_thread do
          assert_equal 0, vt
          sync :foo
          assert_equal 0, vt
        end

        # Sync in main thread
        assert_equal 0, vt
        sync :foo
        assert_equal 0, vt
      end
    end

    def test_sync_no_sleep
      @lang.run do
        assert_equal 0, vt

        in_thread do
          # A live loop with no sleeps and only syncs will functionally act as
          # if it was attempting to sync multiple times at the same beat:
          #
          # live_loop :foo do
          #   sync :bar
          #   sample :bd_haus
          # end
          #
          # So a cue must only trigger a sync once on a given thread.
          sync :foo
          sync :foo
          assert_equal 0.2, vt
        end

        2.times do
          sleep 0.1
          cue :foo
        end
      end
    end

    def test_sync_rational_sleeps
      @lang.run do
        assert_equal 0, vt

        in_thread do
          30.times do
            sleep 0.01
            sync :foo
          end
          assert_equal 0.3, vt
        end

        # 3 sleeps of 0.01/3 seconds would tend to trigger ever so slightly
        # earlier than 0.01 seconds so unless slack is given, the sync may be
        # delayed to the next cue. 
        (30 * (3 + 1)).times do
          sleep 0.01 / 3
          cue :foo
        end
      end
    end

    def test_determinism
      @lang.run do
        seeds = []

        # Repeat experiment a few times to get new threads.
        5.times do
          3.times do
            seeds << Random.new_seed
          end
        end

        # Random sleeps based on the seeds.
        def rsleep(prng)
          case prng.rand(3)
          when 0
          when 1
            sleep 0.01
          when 2
            sleep 0.05
          end
        end

        while seeds do
          # 3 threads - 2 generating cues and 1 syncing to them.
          seed1 = seeds.pop
          seed2 = seeds.pop
          seed3 = seeds.pop

          # Try the same combination of seeds twice.
          results = []
          2.times do
            start = vt

            # Cue generators run while this is set.
            set :go, true

            in_thread do
              prng1 = Random.new(seed1)
              while get :go do
                rsleep(prng1)
                cue :foo
              end
            end

            in_thread do
              result = []
              prng2 = Random.new(seed2)
              25.times do
                rsleep(prng2)
                sync :foo
                timing = vt - start
                # Sometimes the timing is off in the order of ~1e-17 seconds.
                # I assume due to floating point instability.
                result << timing.round(6)
              end
              # Send all the sync timings
              cue :result, result
            end

            in_thread do
              prng3 = Random.new(seed3)
              while get :go do
                rsleep(prng3)
                cue :foo
              end
            end

            results << (sync :result)
            # Stop the cue generators and give them time finish.
            set :go, false
            sleep 0.1
          end

          assert_equal results[0], results[1]
        end

      end
    end

  end
end
