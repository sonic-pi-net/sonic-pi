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
require_relative "../lib/sonicpi/engine_forwards"

# Every /clockwork/ address the GUI handles has to reach it. The engine's
# broadcasts reach it only through the daemon's closed forward list; a few
# addresses Spider sends it itself. A handler with neither route is code that
# never runs: the GUI's switch.done handler was exactly that from May to
# October 2026, and no audio device choice was saved in that time.
module SonicPi
  class EngineForwardsTester < Minitest::Test
    ROOT        = File.expand_path("../../../..", __dir__)
    OSC_HANDLER = File.join(ROOT, "app/api/src/osc/osc_handler.cpp")
    SPIDER      = File.join(ROOT, "app/server/ruby/bin/spider-server.rb")

    def gui_handled
      File.read(OSC_HANDLER).scan(%r{match\("(/clockwork/[^"]+)"\)}).flatten.uniq
    end

    def sent_to_gui_by_spider
      File.read(SPIDER).scan(%r{gui\.send\("(/clockwork/[^"]+)"}).flatten.uniq
    end

    def test_the_gui_handlers_are_found
      # A scan that finds nothing would pass everything below.
      assert_includes gui_handled, "/clockwork/devices/switch.done"
      assert_includes sent_to_gui_by_spider, "/clockwork/midi/out-ports"
    end

    def test_every_clockwork_address_the_gui_handles_reaches_it
      unreachable = gui_handled - ENGINE_TO_GUI_FORWARDS - sent_to_gui_by_spider
      assert_empty unreachable,
                   "The GUI handles these, but nothing delivers them to it. " \
                   "An engine broadcast goes in ENGINE_TO_GUI_FORWARDS " \
                   "(lib/sonicpi/engine_forwards.rb); an engine reply has to " \
                   "be asked for on the daemon's connection to come back."
    end

    def test_every_forward_is_handled_by_the_gui
      # A forward the GUI does not handle is a typo or a handler that has gone.
      assert_empty ENGINE_TO_GUI_FORWARDS - gui_handled
    end

    def test_every_reply_the_gui_handles_is_asked_for_through_the_daemon
      # The engine answers on the asking connection; the GUI's own discards.
      gui_handled.grep(/\.reply\z/).each do |reply|
        request = reply.delete_suffix(".reply")
        asked = GUI_TO_ENGINE_REQUESTS.value?(request) ||
                GUI_TO_ENGINE_REQUESTS.value?("#{request}/get")
        assert asked, "#{reply}: its request is not asked through the daemon"
      end
    end

    def test_the_gui_never_asks_the_engine_directly_what_the_daemon_relays
      sources = Dir[File.join(ROOT, "app/gui/**/*.{cpp,h}")] +
                Dir[File.join(ROOT, "app/api/src/**/*.{cpp,h}")]
      direct = GUI_TO_ENGINE_REQUESTS.values.flat_map do |request|
        sources.select { |f| File.read(f).include?("\"#{request}\"") }
               .map { |f| "#{request} in #{f.delete_prefix(ROOT + '/')}" }
      end
      assert_empty direct, "answers to these are discarded: ask through the daemon"
    end

    def test_spider_still_hears_every_setup
      assert_includes ENGINE_TO_SPIDER_FORWARDS, "/clockwork/setup"
    end

    # The GUI re-attaches its scope, refreshes its device list and readies the
    # Tracks panel on a rebuild. The engine never replays a setup to a
    # registrant, so the first one the daemon sees is a real cold swap too.
    def test_the_gui_hears_every_setup
      assert_includes ENGINE_TO_GUI_FORWARDS, "/clockwork/setup"
    end

    def test_the_daemon_forwards_every_setup_unfiltered
      daemon = File.read(File.join(__dir__, "../bin/daemon.rb"))
      refute_match(/SetupGenerations|rebuild\?/, daemon)
    end
  end
end
