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
  # What Spider relays between the engine and the GUI, and to whom.
  #
  # SuperSonic serves one client on its command port, and Spider is it
  # (EngineConnection): the daemon and the GUI have no connection of their
  # own, so every engine message the GUI hears comes through Spider, and
  # every one it sends goes through Spider, with its token first.
  #
  # CLOSED LISTS, and anything on neither is dropped without a trace. The
  # engine broadcasts to its notify targets, and answers a request on the
  # connection that made it; Spider's connection is the registered one, and
  # the GUI only ever sees what is relayed from here. The first
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

  # The GUI's requests to the daemon that Spider asks the engine
  # (/daemon/... → /clockwork/...): the daemon passes each to Spider, token
  # and all, and Spider sends it on its connection, so the answer comes back
  # there and reaches the GUI through ENGINE_TO_GUI_FORWARDS.
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

  # What the GUI sends the engine itself (SonicPiAPI::SupersonicSendOSC):
  # sent to Spider with the GUI's token first, and sent on by address.
  GUI_TO_ENGINE_DIRECT = [
    "/clockwork/summary",
    "/clockwork/inputs/enable",
    "/clockwork/clock/visibility",
    "/clockwork/clock/tempo/set",
    "/clockwork/clock/audio/publish/set",
    "/clockwork/clock/peer_name/set",
    "/clockwork/clock/audio/input/latency/set",
    "/clockwork/track/list",
    "/clockwork/track/folders",
    "/clockwork/track/folders/add",
    "/clockwork/track/folders/remove",
    "/clockwork/track/scan",
    "/clockwork/track/create",
    "/clockwork/track/remove",
    "/clockwork/track/rename",
    "/clockwork/track/gain",
    "/clockwork/track/mute",
    "/clockwork/track/rig/load",
    "/clockwork/track/rig/save",
    "/clockwork/track/plugin/add",
    "/clockwork/track/plugin/remove",
    "/clockwork/track/plugin/move",
    "/clockwork/track/plugin/bypass",
    "/clockwork/track/plugin/channel",
    "/clockwork/track/plugin/params",
    "/clockwork/track/plugin/param",
    "/clockwork/track/plugin/editor",
  ].freeze
end
