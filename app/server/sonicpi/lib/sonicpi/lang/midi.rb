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

module SonicPi
  module Lang
    module Midi

      include SonicPi::Lang::Support::DocSystem

      def midi_send_timed(*args)
        #TODO remove hardcoded port number
        osmid_o2m_port = 4561
        osc_send "localhost", osmid_o2m_port, *args
      end

      def midi_note_on(n=:e3, vel=nil, opts={})
        # Allow vel to either be passed as a positional parameter or an opt:
        # positional: midi_note_on :e3, 100
        # optional: midi_note_on :e3, vel: 100
        # optional float: midi_note_on :e3, vel_f: 0.7

        if vel.is_a?(Hash) && opts.empty?
          opts = vel
          vel = nil
        end

        if rest? n
          __delayed_message "midi_note_on :rest"
          return nil
        end

        if vel = vel || opts[:velocity] || opts[:vel]
          vel = vel
        elsif vel = opts[:velocity_f] || opts[:vel_f]
          vel = (vel.to_f * 127)
        else
          vel = 127
        end

        channel = (opts[:channel] || opts[:chan] || 1).to_i
        channel = channel.min(0).max(15)
        n = note(n).round.min(0).max(127)
        vel = vel.round.min(0).max(127)

        __delayed_message "midi_note_on #{n}, {vel: #{vel}, channel: #{channel}}"
        midi_send_timed("/*/note_on", channel, n, vel)
        nil
      end
      doc name:           :midi_note_on,
          introduced:     Version.new(2,12,0),
          summary:        "Send MIDI note on message",
          args:           [[:note, :midi]],
          returns:        :nil,
          opts:           {
                             channel: "Channel to send the note event on",
                             chan: "Shorthand for channel:" },

          accepts_block:  false,
          doc:            "Sends a MIDI note on message to *all* connected devices

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_note_on :e3, 12  #=> Sends MIDI note on :e3 with velocity 12 on channel 1",
        "midi_note_on :e3, 12, channel: 3  #=> Sends MIDI note on :e3 with velocity 12 on channel 3"]

      def midi_note_off(n, vel=nil, opts={})
        if vel.is_a?(Hash) && opts.empty?
          opts = vel
          vel = nil
        end

        if rest? n
          __delayed_message "midi_note_off :rest"
          return nil
        end

        if vel = vel || opts[:velocity] || opts[:vel]
          vel = vel
        elsif vel = opts[:velocity_f] || opts[:vel_f]
          vel = (vel.to_f * 127)
        else
          vel = 127
        end

        channel = (opts[:channel] || opts[:chan] || 1).to_i
        channel = channel.min(0).max(15)
        n = note(n).round.min(0).max(127)
        vel = vel.round.min(0).max(127)

        __delayed_message "midi_note_off #{n}, {vel: #{vel}, channel: #{channel}}"
        midi_send_timed("/*/note_off", channel, n, vel)
        nil
      end
      doc name:           :midi_note_off,
          introduced:     Version.new(2,12,0),
          summary:        "Send MIDI note off message",
          args:           [[:note, :midi]],
          returns:        :nil,
          opts:           {
                            channel: "Channel to send the note event on",
                            chan: "Shorthand for channel:"},

          accepts_block:  false,
          doc:            "Sends the MIDI note off message to *all* connected devices

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_note_off :e3, 12  #=> Sends MIDI note off on :e3 with velocity 12 on channel 1",
        "midi_note_off :e3, 12, channel: 3  #=> Sends MIDI note off on :e3 with velocity 12 to channel 3"
]


      def midi_cc(control_num, val=nil, opts={})
        if val.is_a?(Hash) && opts.empty?
          opts = val
          val = nil
        end

        if rest? control_num
          __delayed_message "midi_cc :rest"
          return nil
        end

        channel = (opts[:channel] || opts[:chan] || 1).to_i
        channel = channel.min(0).max(15)

        if val = val || opts[:value] || opts[:val]
          val = val
        elsif val = opts[:value_f] || opts[:val_f]
          val = (val.to_f * 127)
        else
          val = 127
        end

        val = note(val).round.min(0).max(127)
        control_num = note(control_num).round.min(0).max(127)
        __delayed_message "midi_cc #{control_num}, {val: #{val}, channel: #{channel}}"
        midi_send_timed("/*/control_change", channel, control_num, val)
        nil
      end
      doc name:           :midi_cc,
          introduced:     Version.new(2,12,0),
          summary:        "Send MIDI control change message",
          args:           [[:control_num, :midi], [:value, :midi]],
          returns:        :nil,
          opts: {
                  channel: "Channel to send the note event on",
                     chan: "Shorthand for channel:" },

          accepts_block:  false,
          doc:            "Sends a MIDI control change message to *all* connected devices


*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_cc 100, 32  #=> Sends MIDI cc message to control 100 with value 32",
        "midi_cc 100, 32, channel: 5  #=> Sends MIDI cc message to control 100 with value 32 on channel 5"
]


      def midi_raw(a, b, c)
        __delayed_message "midi_raw #{a}, #{b}, #{c}"
        midi_send_timed("/*/raw", a.to_i, b.to_i, c.to_i)
      end
      doc name:           :midi_raw,
          introduced:     Version.new(2,12,0),
          summary:        "Send raw MIDI message",
          args:           [[], ],
          returns:        :nil,
          opts:           nil,
          accepts_block:  false,
          doc:            "Sends the raw MIDI message to *all* connected devices

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_raw 0xb0, 0x7b, 0x0  #=> Sends the MIDI reset command"
]

      def midi_sound_off(opts={})
        __delayed_message "midi_sound_off #{arg_h_pp(opts)}"
        midi_cc 120, 0, opts
      end
      doc name:           :midi_sound_off,
          introduced:     Version.new(2,12,0),
          summary:        "Silence all MIDI devices",
          args:           [],
          returns:        :nil,
          opts: {
                         channel: "Channel to send the sound off message to",
                         chan: "Shorthand for channel:" },
          accepts_block:  false,
          doc:            "Sends MIDI sound off to *all* connected MIDI devices.

All oscillators will turn off, and their volume envelopes are set to zero as soon as possible.



*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_sound_off #=> Silence MIDI devices on channel 1",
        "midi_sound_off, channel: 2 #=> Silence MIDI devices on channel 2"
      ]


      def midi_reset(opts={})
        __delayed_message "midi_reset #{arg_h_pp(opts)}"
        midi_cc 121, 0, opts
      end
      doc name:           :midi_reset,
          introduced:     Version.new(2,12,0),
          summary:        "Reset MIDI devices",
          args:           [],
          returns:        :nil,
          opts: {
                         channel: "Channel to send the midi reset message to",
                         chan: "Shorthand for channel:" },

          accepts_block:  false,
          doc:            "Sends MIDI reset to *all* connected MIDI devices.

All controller values are reset to their default values.


*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_reset #=> Reset MIDI devices on channel 1",
        "midi_reset, channel: 2 #=> Reset MIDI devices on channel 2"
      ]

      def midi_local_control_off(opts={})
        __delayed_message "midi_mode_local_control_off #{arg_h_pp(opts)}"
        midi_cc 122, 0, opts
      end
      doc name:           :midi_local_control_off,
          introduced:     Version.new(2,12,0),
          summary:        "Disable local control on MIDI devices",
          args:           [],
          returns:        :nil,
          opts: {
                         channel: "Channel to send the local control off message to",
                         chan: "Shorthand for channel:" },

          accepts_block:  false,
          doc:            "Sends a MIDI local control off message to *all* connected MIDI devices.

All devices on a given channel will respond only to data received over MIDI. Played data, etc. will be ignored.


*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_local_control_off #=> Disable local control on MIDI devices on channel 1",
        "midi_local_control_off, channel: 2 #=> Disable local control on MIDI devices on channel 2"
      ]

      def midi_local_control_on(opts={})
        __delayed_message "midi_mode_local_control_on #{arg_h_pp(opts)}"
        midi_cc 122, 127, opts
      end
      doc name:           :midi_local_control_on,
          introduced:     Version.new(2,12,0),
          summary:        "Enable local control on MIDI devices",
          args:           [],
          returns:        :nil,
          opts: {
                         channel: "Channel to send the local control on message to",
                         chan: "Shorthand for channel:" },

          accepts_block:  false,
          doc:            "Sends a MIDI local control on message to *all* connected MIDI devices.

All devices on a given channel will respond both to data received over MIDI and played data, etc. See `midi_local_control_off` to disable local control.


*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_local_control_on #=> Enable local control on MIDI devices on channel 1",
        "midi_local_control_on, channel: 2 #=> Enable local control on MIDI devices on channel 2"
      ]


      def midi_mode(mode, opts={})

        channel = opts[:channel] || opts[:chan] || 1
        channel = channel.min(0).max(15)

        case mode
        when :omni_off
          __delayed_message "midi_mode :omni_off"
          midi_cc 124, 0, channel: channel
        when :omni_on
          __delayed_message "midi_mode :omni_on, {channel: #{channel}}"
          midi_cc 125, 0, channel: channel
        when :mono
          num_chans = opts[:num_chans] || 32
          __delayed_message "midi_mode :mono, {num_chans: #{num_chans}, channel: #{channel}}"
          midi_cc 126, num_chans, channel: channel
        when :poly
          __delayed_message "midi_mode :poly, {channel: #{channel}}"
          midi_cc 127, 0, channel: channel
        else
          raise "Unknown special mode for midi_mode: #{mode}. Expected one of: :omni_off, :omni_on, :mono or :poly."
        end
      end
      doc name:           :midi_mode,
          introduced:     Version.new(2,12,0),
          summary:        "Set Omni/Mono/Poly mode",
          args:           [],
          returns:        :nil,
          opts: {
                         channel: "Channel to send the MIDI mode message to",
                         chan: "Shorthand for channel:" },

          accepts_block:  false,
          doc:            "Sends the Omni/Mono/Poly MIDI mode message to *all* connected MIDI devices.

Valid modes are:

:omni_off - Omni Mode Off
:omni_on  - Omni Mode On
:mono     - Mono Mode On (Poly Off). Set num_chans: to be the number of channels to use (Omni Off) or 0 (Omni On). Default for num_chans: is 32.
:poly     - Poly Mode On (Mono Off)

Note that this fn also includes the behaviour of `midi_all_notes_off`.

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_mode :omni_on #=> Omni Mode On",
        "midi_mode :mono, num_chans: 5 #=> Mono Mode On, Omni off using 5 channels.",
        "midi_mode :mono, num_chans: 0 #=> Mono Mode On, Omni on.",
        "midi_mode :mono #=> Mono Mode On, Omni off using 32 channels (the default) ."
      ]



      def midi_all_notes_off(opts={})
        channel = opts[:channel] || opts[:chan] || 1
        channel = channel.min(0).max(15)
        __delayed_message "midi_all_notes_off: #{channel}"
        midi_cc 123, 0, channel: channel
      end
      doc name:           :midi_all_notes_off,
          introduced:     Version.new(2,12,0),
          summary:        "Turn off all notes on MIDI devices",
          args:           [],
          returns:        :nil,
          opts: {
                         channel: "Channel to send the all notes off message to",
                         chan: "Shorthand for channel:" },

          accepts_block:  false,
          doc:            "Sends a MIDI all notes off message to *all* connected MIDI devices.

All devices on a given channel will respond both to data received both over MIDI and Played data, etc. See `midi_local_control_off` off to disable local control.


*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_all_notes_off #=> Turn off all notes on MIDI devices on channel 1",
        "midi_all_notes_off, channel: 2 #=> Turn off all notes on MIDI devices on channel 2"
      ]


      def midi_clock_tick
        __delayed_message "midi_clock_tick"
        midi_send_timed("/*/clock")
      end
      doc name:           :midi_clock_tick,
          introduced:     Version.new(2,12,0),
          summary:        "Send an individual MIDI clock tick",
          args:           [[], ],
          returns:        :nil,
          opts:           nil,
          accepts_block:  false,
          doc:            "Sends the MIDI clock tick to *all* connected MIDI devices

Typical MIDI devices expect the clock to send 24 ticks per quarter note (typically a beat). See `midi_clock_beat` for a simple way of sending all the ticks for a given beat.

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_clock_tick #=> Send an individual clock tick"
      ]

      def midi_start
        __delayed_message "midi_start"
        midi_send_timed("/*/start")
      end
      doc name:           :midi_start,
          introduced:     Version.new(2,12,0),
          summary:        "Send MIDI system message - start",
          args:           [[], ],
          returns:        :nil,
          opts:           nil,
          accepts_block:  false,
          doc:            "Sends the MIDI start system message to *all* connected MIDI devices.



*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_start #=> Send start message to all connected MIDI devices"
      ]

      def midi_clock_beat(dur=1)
        if dur == 1
          times =  [0,
                    0.041666666666666664,
                    0.08333333333333333,
                    0.125,
                    0.16666666666666666,
                    0.20833333333333331,
                    0.24999999999999997,
                    0.29166666666666663,
                    0.3333333333333333,
                    0.375,
                    0.4166666666666667,
                    0.45833333333333337,
                    0.5,
                    0.5416666666666666,
                    0.5833333333333333,
                    0.6249999999999999,
                    0.6666666666666665,
                    0.7083333333333331,
                    0.7499999999999998,
                    0.7916666666666664,
                    0.833333333333333,
                    0.8749999999999997,
                    0.9166666666666663,
                    0.9583333333333329]
        elsif dur == 0.5
          times =  [0,
                    0.020833333333333332,
                    0.041666666666666664,
                    0.0625,
                    0.08333333333333333,
                    0.10416666666666666,
                    0.12499999999999999,
                    0.14583333333333331,
                    0.16666666666666666,
                    0.1875,
                    0.20833333333333334,
                    0.22916666666666669,
                    0.25,
                    0.2708333333333333,
                    0.29166666666666663,
                    0.31249999999999994,
                    0.33333333333333326,
                    0.3541666666666666,
                    0.3749999999999999,
                    0.3958333333333332,
                    0.4166666666666665,
                    0.43749999999999983,
                    0.45833333333333315,
                    0.47916666666666646]
        else
          times = (line 0, dur, steps: 24, inclusive: false)
        end
        __delayed_message "midi_clock_beat"
        time_warp times do
          midi_send_timed("/*/clock")
        end
      end
      doc name:           :midi_clock_beat,
          introduced:     Version.new(2,12,0),
          summary:        "Send a quarter-note's worth of MIDI clock ticks",
          args:           [[], ],
          returns:        :nil,
          opts:           nil,
          accepts_block:  false,
          doc:            "Sends enough MIDI clock ticks for one beat to *all* connected MIDI devices.

Schedules for 24 clock ticks to be sent linearly spread over dur beats.

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_clock_beat #=> Send 24 clock ticks over a period of 1 beat",
        "midi_clock_beat 0.5 #=> Send 24 clock ticks over a period of 0.5 beat"
      ]




      def midi(n=nil, vel=nil, opts={})
        if n.is_a?(Hash)
          opts = n
          n = nil
          vel = nil
        elsif vel.is_a?(Hash)
          opts = vel
          vel = nil
        end

        n = n || opts[:note]

        if rest? n
          __delayed_message "midi :rest"
          return nil
        end

        if vel = vel || opts[:velocity] || opts[:vel]
          vel = vel
        elsif vel = opts[:velocity_f] || opts[:vel_f]
          vel = (vel.to_f * 127)
        else
          vel = 127
        end

        channel = opts[:channel] || opts[:chan] || 1
        channel = channel.min(0).max(15)

        vel = vel.round.min(0).max(127)

        on_val = opts.fetch(:on, 1)

        on on_val do
          return midi_all_notes_off(opts) if n == :off

          dur = opts.fetch(:dur,  1)
          chan = opts[:channel] || opts[:chan] || 1
          rel_vel = opts.fetch(:release_velocity, 127)

          n = note(n).round.min(0).max(127)
          __delayed_message "midi #{n}, {vel: #{vel}, dur: #{dur}, channel: #{chan}}"
          midi_note_on n, vel, channel: channel
          time_warp dur.to_f do
            midi_note_off n, rel_vel, channel: channel
          end
        end
        return nil
      end
      doc name:           :midi,
          introduced:     Version.new(2,12,0),
          summary:        "Trigger and release an external synth via MIDI",
          args:           [[:note, :number], ],
          returns:        :nil,
          opts:           {dur: "Duration of note event in beats",
                           vel:  "Velocity of note as a MIDI number"},
          accepts_block:  false,
          doc:            "Sends a MIDI note on event to *All* connected MIDI devices and then after dur beats sends a MIDI note off event. Ensures MIDI trigger is synchronised with standard calls to play and sample. Co-operates completely with Sonic Pi's timing system including time_warp.

If `note` is specified as `:off` then all notes will be turned off (same as `midi_all_notes_off`).

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi :e1, dur: 0.3, amp: 0.5, channel: 3",
        "midi :off, channel: 3 #=> Turn off all notes on channel 3"
]

    end
  end
end
