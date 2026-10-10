#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "./setup_test"
require_relative "../lib/sonicpi/plugins"

module SonicPi
  # Plugin hosting is a build's, and the GUI says whether this one has it, as
  # SONIC_PI_PLUGINS, to everything it starts.
  class PluginsTester < Minitest::Test
    def with_plugins(value)
      before = ENV["SONIC_PI_PLUGINS"]
      value.nil? ? ENV.delete("SONIC_PI_PLUGINS") : ENV["SONIC_PI_PLUGINS"] = value
      yield
    ensure
      before.nil? ? ENV.delete("SONIC_PI_PLUGINS") : ENV["SONIC_PI_PLUGINS"] = before
    end

    def test_none_unless_the_build_says_so
      with_plugins(nil) { refute Plugins.available? }
      %w[0 false no off nope].each { |v| with_plugins(v) { refute Plugins.available?, v } }
      %w[1 true yes on TRUE On].each { |v| with_plugins(v) { assert Plugins.available?, v } }
    end
  end
end

require_relative "../lib/sonicpi/studio"
require_relative "../lib/sonicpi/runtime"

module SonicPi
  # A build without plugin hosting has no tracks, so nothing asks the engine
  # about them: not the studio at boot or after a device change, not Stop.
  class TrackMessagesWithoutPluginsTester < Minitest::Test
    def setup
      @before = ENV.delete("SONIC_PI_PLUGINS")
      @server = mock
      @studio = Studio.allocate
      @studio.instance_variable_set(:@server, @server)
      @runtime = Object.new
      @runtime.extend(RuntimeMethods)
      @runtime.instance_variable_set(:@mod_sound_studio, @studio)
      @studio.define_singleton_method(:server) { @server }
    end

    def teardown
      @before.nil? ? ENV.delete("SONIC_PI_PLUGINS") : ENV["SONIC_PI_PLUGINS"] = @before
    end

    def test_without_plugins_nothing_asks_about_tracks
      @server.expects(:osc).never
      @server.expects(:track_all_notes_off).never
      @studio.request_track_list
      @runtime.send(:__track_flush!)
    end

    def test_with_plugins_the_studio_and_stop_still_do
      ENV["SONIC_PI_PLUGINS"] = "1"
      @server.expects(:osc).with("/clockwork/track/list")
      @server.expects(:track_all_notes_off).with(nil)
      @studio.request_track_list
      @runtime.send(:__track_flush!)
    end
  end
end
