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

module SonicPi
  # What the daemon relays from the engine, and to whom.
  #
  # CLOSED LISTS, and anything on neither is dropped without a trace. The
  # engine broadcasts to its notify targets, and answers a request on the
  # connection that made it; the daemon's connection is the registered one,
  # and the GUI and Spider only ever see what is relayed from here. The first
  # plugin broadcasts were sent correctly by the engine and died here, which
  # looked from the GUI exactly like a plugin that never loaded; the GUI's
  # switch.done handler went unheard from May to October 2026, so no audio
  # device choice was ever saved. test/test_engine_forwards.rb checks the GUI
  # list against every /clockwork/ address the GUI handles. The track
  # addresses are everything the Tracks panel hears (harness/docs/TRACKS.md,
  # "The OSC surface").
  ENGINE_TO_GUI_FORWARDS = [
    "/clockwork/statechange",
    "/clockwork/info",
    "/clockwork/devices",
    "/clockwork/device-table",
    "/clockwork/input-devices",
    "/clockwork/devices/reopen.reply",
    "/clockwork/devices/reopen.done",
    "/clockwork/devices/switch.done",
    # A rebuild (a cold swap): the GUI re-attaches its view of the engine.
    # An event, never replayed to a registrant, so every one is a rebuild.
    "/clockwork/setup",
    # Answers to the GUI's /daemon/clock/audio/* requests, which the daemon
    # asks on its own connection so the answers come back here.
    "/clockwork/clock/audio/channels.reply",
    "/clockwork/clock/audio/inputs.reply",
    "/clockwork/track/list",
    "/clockwork/track/state",
    "/clockwork/track/folders",
    "/clockwork/track/plugins",
    "/clockwork/track/plugin/params",
    "/clockwork/track/plugin/param/edit",
    "/clockwork/track/plugin/param/value",
    "/clockwork/track/error",
  ].freeze

  # The GUI's requests the daemon asks the engine on its own connection
  # (/daemon/... → /clockwork/...), with the GUI's token first. The engine
  # answers a request on the connection it came in on, and the GUI's own
  # engine connection discards whatever comes back on it: a request whose
  # answer the GUI wants goes through here, and its answer through
  # ENGINE_TO_GUI_FORWARDS.
  GUI_TO_ENGINE_REQUESTS = {
    "/daemon/audio/switch-device"   => "/clockwork/devices/switch",
    "/daemon/audio/switch-driver"   => "/clockwork/drivers/switch",
    # devices/report (portless): registers this daemon connection as the
    # report audience and triggers a device table broadcast — the
    # stream-transport equivalent of the GUI's old direct UDP-port
    # registration.
    "/daemon/audio/request-devices" => "/clockwork/devices/report",
    "/daemon/audio/reopen-device"   => "/clockwork/devices/reopen",
    "/daemon/clock/audio/channels"  => "/clockwork/clock/audio/channels/get",
    "/daemon/clock/audio/inputs"    => "/clockwork/clock/audio/inputs/get",
  }.freeze

  # Spider rebuilds the studio after every cold swap.
  ENGINE_TO_SPIDER_FORWARDS = [
    "/clockwork/setup",
  ].freeze
end
