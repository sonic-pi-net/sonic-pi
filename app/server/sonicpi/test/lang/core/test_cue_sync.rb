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
require_relative "../../../lib/sonicpi/incomingevents"

Thread.abort_on_exception = true

module SonicPi
  class CueSyncTester < Minitest::Test

    def setup
      events = IncomingEvents.new
      @mock_core = Object.new
      @mock_core.extend(Lang::Core)
      @mock_core.stubs(:current_job_id).returns(100)
      @mock_core.stubs(:__delayed_highlight_message).returns(true)
      @mock_core.stubs(:__delayed_highlight_message).returns(true)
      @mock_core.stubs(:__delayed_highlight3_message).returns(true)
      @mock_core.stubs(:__delayed_highlight2_message).returns(true)
      @mock_core.stubs(:__schedule_delayed_blocks_and_messages!).returns(true)
      @mock_core.stubs(:__events).returns(events)
    end

    def send_rcv_cue_sync(cue_id, *args, &blk)
      p = Promise.new

      t = Thread.new do
        res = @mock_core.sync cue_id do
          p.deliver! true
        end
        blk.call(res) if block_given?
      end

      p.get
      @mock_core.cue cue_id, *args
      t.join
    end

    def send_rcv_cue_sync_multi(cue_id, sync_ids, *args, &blk)
      p = Promise.new

      t = Thread.new do
        res = @mock_core.sync sync_ids do
          p.deliver! true
        end
        blk.call(res) if block_given?
      end

      p.get
      @mock_core.cue cue_id, *args
      t.join
    end


    def test_basic_cue_with_no_args
      send_rcv_cue_sync :foo do |res|
        assert_equal SonicPi::Core::SPVector.new([]), res
      end

      send_rcv_cue_sync :bar do |res|
        assert_equal SonicPi::Core::SPVector.new([]), res
      end
    end


    def test_basic_cue_with_list_args
      send_rcv_cue_sync :foo, 1, "bar", 3.38 do |res|
        a, b, c = res
        assert_equal a, 1
        assert_equal b, "bar"
        assert_equal c, 3.38
      end

      send_rcv_cue_sync :foo, 1, 2, 3 do |res|
        assert_equal res[0], 1
        assert_equal res[1], 2
        assert_equal res[2], 3
      end

      send_rcv_cue_sync :bar do |res|
        assert_equal SonicPi::Core::SPVector.new([]), res
      end
    end

    def test_basic_cue_with_map_args
      send_rcv_cue_sync :foo, a: 1, b: 2, c: 3 do |res|
        a, b, c = res
        assert_equal a, 1
        assert_equal b, 2
        assert_equal c, 3
      end

      send_rcv_cue_sync :bar, a: 1, b: 2, c: 3 do |res|
        assert_equal res[:a], 1
        assert_equal res[:b], 2
        assert_equal res[:c], 3
      end
    end

    def test_multi_cues
      send_rcv_cue_sync_multi :foo, [:bar, :foo], a: 1, b: 2, c: 3 do |res|
        a, b, c = res
        assert_equal a, 1
        assert_equal b, 2
        assert_equal c, 3
      end
    end
  end
end
