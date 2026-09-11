#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#++

require_relative "setup_test"
require_relative "../lib/sonicpi/studio"

module SonicPi
  # The studio's track monitors: a track is heard on the main mix until a
  # live_track claims it; Stop parks it silent; playing into it or
  # `live_track :name, :stop` puts it back.
  class TrackMonitorsTest < Minitest::Test
    def setup
      @server = mock
      @studio = Studio.allocate
      @studio.instance_variable_set(:@server, @server)
      @studio.instance_variable_set(:@rebooting, false)
      @studio.instance_variable_set(:@mixer_group, 10)
      @studio.instance_variable_set(:@mixer_bus, 16)
      @studio.instance_variable_set(:@paused, false)
      @studio.instance_variable_set(:@recording_mutex, Mutex.new)
      @studio.instance_variable_set(:@tracks_mut, Mutex.new)
      @studio.instance_variable_set(:@tracks, {"surge" => {id: 7, return: 2}})
      @studio.instance_variable_set(:@track_monitors, {})
      @studio.instance_variable_set(:@track_claimed, Set.new)
      @studio.instance_variable_set(:@track_parked, Set.new)
      @studio.instance_variable_set(:@track_monitor_mut, Mutex.new)
    end

    def monitors
      @studio.instance_variable_get(:@track_monitors)
    end

    def expect_monitor
      node = mock
      @server.expects(:trigger_synth).with { |*a| a[2] == "sonic-pi-live_audio_stereo" && a[3]["input"] == 3 }.returns(node).once
      node
    end

    def test_a_track_is_heard_until_claimed_and_stop_parks_it_silent
      node = expect_monitor
      @studio.send(:__sync_track_monitors)
      assert_equal [7], monitors.keys
      node.expects(:kill).with(false)
      @studio.track_monitor_claim(7)
      assert_empty monitors
      # Stop: the node dies, the track parks, and a sync (a track list
      # arriving, say) does not bring the monitor back.
      @studio.track_monitor_park(7)
      @server.expects(:trigger_synth).never
      @studio.send(:__sync_track_monitors)
      assert_empty monitors
    end

    def test_playing_into_a_parked_track_brings_it_back
      node = expect_monitor
      @studio.send(:__sync_track_monitors)
      node.expects(:kill)
      @studio.track_monitor_claim(7)
      @studio.track_monitor_park(7)
      expect_monitor
      @studio.track_touched("surge")
      assert_equal [7], monitors.keys
      # Touching a track that is heard already changes nothing.
      @server.expects(:trigger_synth).never
      @studio.track_touched("surge")
    end

    def test_touching_a_claimed_live_track_leaves_it_with_the_live_track
      node = expect_monitor
      @studio.send(:__sync_track_monitors)
      node.expects(:kill)
      @studio.track_monitor_claim(7)
      @server.expects(:trigger_synth).never
      @studio.track_touched("surge")
      assert_empty monitors
    end

    def test_live_track_stop_hands_the_track_back_whichever_order_the_park_arrives
      node = expect_monitor
      @studio.send(:__sync_track_monitors)
      node.expects(:kill)
      @studio.track_monitor_claim(7)
      expect_monitor
      @studio.track_monitor_release(7)
      assert_equal [7], monitors.keys
      # The node's own park, arriving after the release, is not a claim.
      @studio.track_monitor_park(7)
      @server.expects(:trigger_synth).never
      @studio.send(:__sync_track_monitors)
      assert_equal [7], monitors.keys
    end

    def test_a_park_that_arrives_before_the_release_is_undone_by_it
      node = expect_monitor
      @studio.send(:__sync_track_monitors)
      node.expects(:kill)
      @studio.track_monitor_claim(7)
      @studio.track_monitor_park(7)
      expect_monitor
      @studio.track_monitor_release(7)
      assert_equal [7], monitors.keys
    end
  end
end
