#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/sonic-pi-net/sonic-pi
# License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "supersonic_gamepad_comms"

module SonicPi
  # Spider-side game-controller API over SupersonicGamepadComms — a sibling of
  # MidiAPI. SuperSonic owns the device IO (gilrs: evdev / IOKit / XInput, with
  # the SDL controller-mapping database) and pushes normalised /gamepad/in/*
  # events; here they are re-emitted as Sonic Pi cues so controllers are
  # sync-able like MIDI devices:
  #
  #   /gamepad:<pad>/button/<name>        [pressed, value]  every change
  #   /gamepad:<pad>/button/<name>/down   [value]           press edge
  #   /gamepad:<pad>/button/<name>/up     [value]           release edge
  #   /gamepad:<pad>/axis/<name>          [value]           stick move (-1..1, up/right +)
  #   /gamepad/devices                    [name, ...]       connect/disconnect
  #
  # Button names follow the W3C standard-mapping vocabulary (south, east, west,
  # north, left_shoulder, right_trigger, dpad_up, start, mode, ...); axes are
  # left_x/left_y/right_x/right_y. Button values are 0..1 (analog triggers
  # sweep). SuperSonic already deduplicates, deadzones and quantises, so every
  # cue is a real state change.
  class GamepadAPI

    def initialize(supersonic_host, supersonic_port, handlers)
      @gamepad_comms = SonicPi::SupersonicGamepadComms.new(supersonic_host, supersonic_port)
      @internal_cue_handler = handlers[:internal_cue]
      @updated_gamepads_handler = handlers[:updated_gamepads]
      # Last-seen pressed state per [pad, button], to derive the /down and /up
      # edge cues from the (pressed, value) stream.
      @pressed = {}

      add_supersonic_gamepad_handlers!
      @gamepad_comms.subscribe_to_notifications!
      @gamepad_comms.send("/gamepad/devices/list")   # prime the device list
    end

    def gamepad_system_start!
      @gamepad_comms.subscribe_to_notifications!
    end

    def gamepad_system_stop!
      @gamepad_comms.unsubscribe_from_notifications!
    end

    def gamepad_refresh_devices!
      @gamepad_comms.send("/gamepad/refresh")
    end

    # Dual-motor rumble (magnitudes 0..1), best-effort: pads or platforms
    # without force feedback ignore it. duration_ms <= 0 plays until
    # gamepad_rumble_stop!. Immediate (controllers have no scheduled out path).
    def gamepad_rumble!(pad, strong, weak, duration_ms)
      @gamepad_comms.send("/gamepad/out/rumble", pad.to_s, strong.to_f, weak.to_f, duration_ms.to_i)
    end

    def gamepad_rumble_stop!(pad)
      @gamepad_comms.send("/gamepad/out/rumble_stop", pad.to_s)
    end

    private

    def cue(path, args)
      @internal_cue_handler.call(path, args) if @internal_cue_handler
    end

    def add_supersonic_gamepad_handlers!
      # Buttons: /gamepad/in/button <pad> <button> <pressed> <value>.
      # Every change re-emits as the raw cue; press/release transitions also
      # emit /down and /up so a `sync` can wait on a specific edge.
      @gamepad_comms.add_method("/gamepad/in/button") do |args|
        pad, button, pressed, value = args
        base = "/gamepad:#{pad}/button/#{button}"
        cue(base, [pressed, value])

        key = [pad, button]
        was = @pressed[key]
        @pressed[key] = (pressed == 1)
        if pressed == 1 && was != true
          cue("#{base}/down", [value])
        elsif pressed == 0 && was
          cue("#{base}/up", [value])
        end
      end

      # Axes: /gamepad/in/axis <pad> <axis> <value>.
      @gamepad_comms.add_method("/gamepad/in/axis") do |args|
        pad, axis, value = args
        cue("/gamepad:#{pad}/axis/#{axis}", [value])
      end

      # Device list: /gamepad/devices[.reply] = n [name enabled]*. Pushed on
      # connect/disconnect; also re-emitted as a cue so code can react to a
      # controller appearing.
      ["/gamepad/devices", "/gamepad/devices.reply"].each do |addr|
        @gamepad_comms.add_method(addr) do |args|
          pads = parse_devices(args)
          # Drop edge state for departed pads — SuperSonic doesn't synthesise
          # releases on disconnect, so a button held at unplug would otherwise
          # swallow the first /down after a reconnect.
          @pressed.delete_if { |(pad, _button), _| !pads.include?(pad) }
          @updated_gamepads_handler.call(pads) if @updated_gamepads_handler
          cue("/gamepad/devices", pads) if addr == "/gamepad/devices"
        end
      end
    end

    # Extract the pad-name list from a /gamepad/devices payload
    # (n [name enabled]*): drop the count, keep the names.
    def parse_devices(args)
      args.drop(1).each_slice(2).map(&:first)
    end
  end
end
