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
require_relative "../lib/sonicpi/scsynthexternal"

module SonicPi
  # Regression for #3551. When SuperSonic is up but never answers the boot
  # handshake - its audio device stopped ticking, so nothing drains the OSC
  # ring - spider used to call Kernel#exit here. That surfaced to the user as
  # "Server Exception: exit" plus a backtrace, and to the GUI as "GUI was
  # unable to connect to the Ruby server", neither of which names the audio
  # device or gives the user anything to act on.
  class SCSynthBootErrorTester < Minitest::Test

    def unbooted_scsynth
      # Port 0 is never listening, so the handshake can only time out.
      scsynth = SCSynthExternal.allocate
      scsynth.instance_variable_set(:@hostname, "127.0.0.1".freeze)
      scsynth.instance_variable_set(:@send_port, 0)
      scsynth
    end

    def test_boot_timeout_raises_boot_error_rather_than_exiting
      assert_raises(SCSynthExternal::BootError) do
        unbooted_scsynth.__send__(:connect_to_server, 0.2)
      end
    end

    def test_boot_error_names_the_audio_server_and_the_log
      e = assert_raises(SCSynthExternal::BootError) do
        unbooted_scsynth.__send__(:connect_to_server, 0.2)
      end

      assert_includes e.message, "audio server"
      assert_includes e.message, "audio device is not responding"
      assert_includes e.message, "supersonic.log"
    end

    def test_boot_error_reports_the_timeout_it_actually_waited
      e = assert_raises(SCSynthExternal::BootError) do
        unbooted_scsynth.__send__(:connect_to_server, 0.2)
      end

      assert_includes e.message, "0.2 seconds"
    end

    # Regression: with SuperSonic 0.89.0 on macOS, scsynth's 128 MB real-time
    # pool did not fit the engine's heap, so the engine came up with no synth
    # inside it. Spider waited 30 s for a handshake that never came and blamed
    # the audio device. The engine now answers, and its state replay says
    # "error" with the reason - spider must stop at once and pass that on.
    def test_an_engine_in_error_fails_the_boot_with_its_reason
      reason = "scsynth did not start: World_New: RT pool of 134217728 bytes " \
               "was asked for and the host cannot provide it"
      e = assert_raises(SCSynthExternal::BootError) do
        unbooted_scsynth.__send__(:raise_if_engine_failed!, ["error", reason])
      end

      assert_includes e.message, reason
      assert_includes e.message, "supersonic.log"
      refute_includes e.message, "audio device is not responding"
    end

    def test_a_running_engine_does_not_fail_the_boot
      unbooted_scsynth.__send__(:raise_if_engine_failed!, ["running", "snapshot"])
      unbooted_scsynth.__send__(:raise_if_engine_failed!, nil)
    end
  end
end
