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

      def midi_available_ports
        ['*'].map{|el| el.freeze}.freeze
      end




      def use_midi_channels(chan)
        __thread_locals.set(:sonic_pi_midi_channels, v)
      end




      def use_midi_ports(*filters_and_procs)
        if is_list_like?(filters_and_procs) && filters_and_procs.size == 1 && filters_and_procs[0] == '*'
          __thread_locals.set(:sonic_pi_midi_ports, '*'.freeze)
          return '*'
        end

        filters_and_procs = [filters_and_procs] unless is_list_like?(filters_and_procs)
        string_filters = []
        non_string_f_and_ps = []
        filters_and_procs.each do |fp|
          if fp.is_a? String
            string_filters << fp
          else
            non_string_f_and_ps << fp
          end
        end

        unless string_filters.empty?
          string_filters.map!{|sf| Regexp.escape(sf)}
          string_filter_regexp = Regexp.new('.*' + string_filters.join('.*') + '.*')
          non_string_f_and_ps.unshift string_filter_regexp
        end

        candidates = midi_available_ports.clone.to_a
        non_string_f_and_ps.each do |f|
          case f
          when Symbol
            candidates.keep_if do |c|
              c == f.to_s
            end
          when Regexp
            candidates.keep_if do |c|
              f.match c
            end
          when Fixnum
            unless candidates.empty?
              candidates = [candidates[f % candidates.size]]
            end
          when NilClass
            # Do nothing
          when Proc
            raise "MIDI Port Filter Proc needs to accept 1 argument only. Found #{block.arity}" unless f.arity == 1
            found_proc = true
            candidates = f.call(candidates)
            candidates = [candidates] unless is_list_like?(candidates)
          else
            raise "Unknown MIDI port filter type: #{f.class} - got: #{f.inspect}"
          end
        end
        __thread_locals.set(:sonic_pi_midi_ports, candidates.freeze)
      end




      def current_midi_channels
        __thread_locals.get(:sonic_pi_midi_channel, ['*'])
      end




      def current_midi_ports
        __thread_locals.get(:sonic_pi_midi_ports, ['*'])
      end




      def midi_note_on(*args)
        params, opts = split_params_and_merge_opts_array(args)
        n, vel = *params

        if rest? n
          __delayed_message "midi_note_on :rest"
          return nil
        end

        n = normalise_transpose_and_tune_note_from_args(n, opts)

        channels = __resolve_midi_channels(opts)
        ports    = __resolve_midi_ports(opts)
        vel      = __resolve_midi_velocity(vel, opts)
        n        = n.round.min(0).max(127)
        chan     = pp_el_or_list(channels)
        port     = pp_el_or_list(ports)

        ports.each do |p|
          channels.each do |c|
            __midi_send_timed_pc("/note_on", p, c, [n, vel])
          end
        end
        __delayed_message "midi_note_on #{n}, #{vel}, channel: #{chan}, port: #{port}"
        nil
      end
      doc name:           :midi_note_on,
          introduced:     Version.new(2,12,0),
          summary:        "Send MIDI note on message",
          args:           [[:note, :midi], [:velocity, :midi]],
          returns:        :nil,
          opts:           {
                           channel: "Channel to send to",
                           port: "MIDI port to send to",
                           vel_f: "Velocity as a value between 0 and 1 (will be converted to a MIDI velocity)"},
          accepts_block:  false,
          doc:            "Sends a MIDI note on message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_note_on :e3, 12  #=> Sends MIDI note on :e3 with velocity 12 to all channels",
        "midi_note_on :e3, 12, channel: 3  #=> Sends MIDI note on :e3 with velocity 12 on channel 3",
        "midi_note_on :e3, vel_f: 0.8 #=> Sends MIDI note on for :e1 with velocity 102"]




      def midi_note_off(*args)
        params, opts = split_params_and_merge_opts_array(args)
        n, vel = *params

        if rest? n
          __delayed_message "midi_note_off :rest"
          return nil
        end

        n = normalise_transpose_and_tune_note_from_args(n, opts)

        on_val = opts.fetch(:on, 1)

        on on_val do
          channels = __resolve_midi_channels(opts)
          ports    = __resolve_midi_ports(opts)
          vel      = __resolve_midi_velocity(vel, opts)
          n        = note(n).round.min(0).max(127)
          chan     = pp_el_or_list(channels)
          port     = pp_el_or_list(ports)

          ports.each do |p|
            channels.each do |c|
              __midi_send_timed_pc("/note_off", p, c, [n, vel])
            end
          end
          __delayed_message "midi_note_off #{n}, #{vel}, port: #{port}, channel: #{chan}"
        end
        nil
      end
      doc name:           :midi_note_off,
          introduced:     Version.new(2,12,0),
          summary:        "Send MIDI note off message",
          args:           [[:note, :midi], [:release_velocity, :midi]],
          returns:        :nil,
          opts:           {
                           channel: "Channel to send to",
                           port: "MIDI port to send to",
                           velicity: "Release velocity as a MIDI number",
                           vel_f: "Release velocity as a value between 0 and 1 (will be converted to a MIDI velocity)"},
          accepts_block:  false,
          doc:            "Sends the MIDI note off message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
          "midi_note_off :e3, 12  #=> Sends MIDI note off on :e3 with velocity 12 on all channels",
          "midi_note_off :e3, 12, channel: 3  #=> Sends MIDI note off on :e3 with velocity 12 to channel 3",
          "midi_note_off :e3, vel_f: 0.8 #=> Sends MIDI note off for :e1 with velocity 102"
]




      def midi_cc(*args)
        params, opts = split_params_and_merge_opts_array(args)
        control_num, val = *params

        if rest? control_num
          __delayed_message "midi_cc :rest"
          return nil
        end

        on_val = opts.fetch(:on, 1)

        on on_val do
          channels    = __resolve_midi_channels(opts)
          ports       = __resolve_midi_ports(opts)
          val         = __resolve_midi_val(val, opts)
          control_num = note(control_num).round.min(0).max(127)
          chan        = pp_el_or_list(channels)
          port        = pp_el_or_list(ports)

          ports.each do |p|
            channels.each do |c|
              __midi_send_timed_pc("/control_change", p, c, [control_num, val])
            end
          end
          __delayed_message "midi_cc #{control_num}, #{val}, port: #{port}, channel: #{chan}"
        end
        nil
      end
      doc name:           :midi_cc,
          introduced:     Version.new(2,12,0),
          summary:        "Send MIDI control change message",
          args:           [[:control_num, :midi], [:value, :midi]],
          returns:        :nil,
          opts:           {
                           channel: "Channel to send to",
                           port: "MIDI port to send to",
                           value: "Control value as a MIDI number",
                           val_f: "Control value as a value between 0 and 1 (will be converted to a MIDI velocity)"},
          accepts_block:  false,
          doc:            "Sends a MIDI control change message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.


*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_cc 100, 32  #=> Sends MIDI cc message to control 100 with value 32 to all ports and channels",
        "midi_cc 100, 32, channel: 5  #=> Sends MIDI cc message to control 100 with value 32 on channel 5",
        "midi_cc 100, val_f: 0.8, channel: 5  #=> Sends MIDI cc message to control 100 with value 102 on channel 5",
        "midi_cc 100, value: 102, channel: 5  #=> Sends MIDI cc message to control 100 with value 102 on channel 5"
]




      def midi_raw(a, b, c, opts={})
        ports = __resolve_midi_ports(opts)
        ports.each do |p|
          __midi_send_timed("/#{p}/raw", a.to_i, b.to_i, c.to_i)
        end
        port = pp_el_or_list(ports)
        __delayed_message "midi_raw #{a}, #{b}, #{c}, port: #{port}"
        nil
      end
      doc name:           :midi_raw,
          introduced:     Version.new(2,12,0),
          summary:        "Send raw MIDI message",
          args:           [[], ],
          returns:        :nil,
          opts:           {port: "Port or ports to send the MIDI message to"},
          accepts_block:  false,
          doc:            "Sends the raw MIDI message to *all* connected MIDI devices. Gives you direct access to the bytes of a MIDI message. Typically this should be rarely used - prefer the other `midi_` fns where possible.

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_raw 0xb0, 0x7b, 0x0  #=> Sends the MIDI reset command"
]




      def midi_sound_off(opts={})
        ports    = __resolve_midi_ports(opts)
        channels = __resolve_midi_channels(opts)
        port     = pp_el_or_list(ports)
        chan     = pp_el_or_list(channels)

        ports.each do |p|
         channels.each do |c|
            __midi_send_timed_pc("/control_change", p, c, [120, 0])
          end
        end
        __delayed_message "midi_sound_off port: #{port}, channel: #{chan}"
        nil
      end
      doc name:           :midi_sound_off,
          introduced:     Version.new(2,12,0),
          summary:        "Silence all MIDI devices",
          args:           [],
          returns:        :nil,
          opts: {
                           channel: "Channel to send the sound off message to",
                           port: "MIDI port to send to"},
          accepts_block:  false,
          doc:            "Sends a MIDI sound off message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

All oscillators will turn off, and their volume envelopes are set to zero as soon as possible.

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_sound_off #=> Silence MIDI devices on all ports and channels",
        "midi_sound_off channel: 2 #=> Silence MIDI devices on channel 2"
      ]




      def midi_reset(opts={})
        ports    = __resolve_midi_ports(opts)
        channels = __resolve_midi_channels(opts)
        port     = pp_el_or_list(ports)
        chan     = pp_el_or_list(channels)

        ports.each do |p|
          channels.each do |c|
            __midi_send_timed_pc("/control_change", p, c, [121, 0])
          end
        end
        __delayed_message "midi_reset port: #{port}, channel: #{chan}"
        nil
      end
      doc name:           :midi_reset,
          introduced:     Version.new(2,12,0),
          summary:        "Reset MIDI devices",
          args:           [],
          returns:        :nil,
          opts: {
                  channel: "Channel to send the midi reset message to",
                  port: "MIDI port to send to"},

          accepts_block:  false,
          doc:            "Sends a MIDI reset message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

All controller values are reset to their defaults.

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_reset #=> Reset MIDI devices on all channels (and ports)",
        "midi_reset channel: 2 #=> Reset MIDI devices on channel 2"
      ]




      def midi_local_control_off(opts={})
        ports    = __resolve_midi_ports(opts)
        channels = __resolve_midi_channels(opts)
        port     = pp_el_or_list(ports)
        chan     = pp_el_or_list(channels)

        ports.each do |p|
          channels.each do |c|
            __midi_send_timed_pc("/control_change", p, c, [122, 0])
          end
        end
        __delayed_message "midi_mode_local_control_off port: #{port}, channel: #{chan}"
        nil
      end
      doc name:           :midi_local_control_off,
          introduced:     Version.new(2,12,0),
          summary:        "Disable local control on MIDI devices",
          args:           [],
          returns:        :nil,
          opts: {
                   channel: "Channel to send the local control off message to",
                   port: "MIDI port to send to"},

          accepts_block:  false,
          doc:            "Sends a MIDI local control off message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

All devices on a given channel will respond only to data received over MIDI. Played data, etc. will be ignored. See `midi_local_control_on` to enable local control.

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_local_control_off #=> Disable local control on MIDI devices on all channels (and ports)",
        "midi_local_control_off channel: 2 #=> Disable local control on MIDI devices on channel 2"
      ]




      def midi_local_control_on(opts={})
        ports    = __resolve_midi_ports(opts)
        channels = __resolve_midi_channels(opts)
        port     = pp_el_or_list(ports)
        chan     = pp_el_or_list(channels)

        ports.each do |p|
          channels.each do |c|
            __midi_send_timed_pc("/control_change", p, c, [122, 127])
          end
        end
        __delayed_message "midi_mode_local_control_on port: #{port}, channel: #{chan}"
        nil
      end
      doc name:           :midi_local_control_on,
          introduced:     Version.new(2,12,0),
          summary:        "Enable local control on MIDI devices",
          args:           [],
          returns:        :nil,
          opts: {
                   channel: "Channel to send the local control on message to",
                   port: "MIDI port to send to"},

          accepts_block:  false,
          doc:            "Sends a MIDI local control on message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

All devices on a given channel will respond both to data received over MIDI and played data, etc. See `midi_local_control_off` to disable local control.

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_local_control_on #=> Enable local control on MIDI devices on all channels (and ports)",
        "midi_local_control_on channel: 2 #=> Enable local control on MIDI devices on channel 2"
      ]




      def midi_mode(mode, opts={})
        channels = __resolve_midi_channels(opts)
        ports    = __resolve_midi_ports(opts)
        port     = pp_el_or_list(ports)
        chan     = pp_el_or_list(channels)

        case mode
        when :omni_off
          ports.each do |p|
            channels.each do |c|
              __midi_send_timed_pc("/control_change", p, c, [124, 0])
            end
          end
          __delayed_message "midi_mode :omni_off, port: #{port}, channel: #{chan}"
        when :omni_on
          ports.each do |p|
            channels.each do |c|
              __midi_send_timed_pc("/control_change", p, c, [125, 0])
            end
          end
          __delayed_message "midi_mode :omni_on, port: #{port}, channel: #{chan}"
        when :mono
          num_chans = opts[:num_chans] || 16
          ports.each do |p|
            channels.each do |c|
              __midi_send_timed_pc("/control_change", p, c, [126, num_chans])
            end
          end
          __delayed_message "midi_mode :mono, num_chans: #{num_chans}, port: #{port}, channel: #{chan}}"
        when :poly
          ports.each do |p|
            channels.each do |c|
              __midi_send_timed_pc("/control_change", p, c, [127, 0])
            end
          end
          __delayed_message "midi_mode :poly, port: #{port}, channel: #{chan}}"
        else
          raise "Unknown special mode for midi_mode: #{mode}. Expected one of: :omni_off, :omni_on, :mono or :poly."
        end
        nil
      end
      doc name:           :midi_mode,
          introduced:     Version.new(2,12,0),
          summary:        "Set Omni/Mono/Poly mode",
          args:           [],
          returns:        :nil,
          opts: {
                         channel: "Channel to send the MIDI mode message to",
                         port: "MIDI port to send to",
                         num_chans: "Used in mono mode only - Number of channels (defaults to 16)"},

          accepts_block:  false,
          doc:            "Sends the Omni/Mono/Poly MIDI mode message to *all* connected MIDI devices.

Valid modes are:

:omni_off - Omni Mode Off
:omni_on  - Omni Mode On
:mono     - Mono Mode On (Poly Off). Set num_chans: to be the number of channels to use (Omni Off) or 0 (Omni On). Default for num_chans: is 16.
:poly     - Poly Mode On (Mono Off)

Note that this fn also includes the behaviour of `midi_all_notes_off`.

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_mode :omni_on #=> Turn Omni Mode On on all ports and channels",
        "midi_mode :mono, num_chans: 5 #=> Mono Mode On, Omni off using 5 channels.",
        "midi_mode :mono, num_chans: 0 #=> Mono Mode On, Omni on.",
        "midi_mode :mono #=> Mono Mode On, Omni off using 16 channels (the default) ."
      ]




      def midi_all_notes_off(opts={})
        channels = __resolve_midi_channels(opts)
        ports    = __resolve_midi_ports(opts)
        port     = pp_el_or_list(ports)
        chan     = pp_el_or_list(channels)

        ports.each do |p|
          channels.each do |c|
            __midi_send_timed_pc("/control_change", p, c, [123, 0])
          end
        end
        __delayed_message "midi_all_notes_off port: #{port}, channel: #{chan}"
      end
      doc name:           :midi_all_notes_off,
          introduced:     Version.new(2,12,0),
          summary:        "Turn off all notes on MIDI devices",
          args:           [],
          returns:        :nil,
          opts: {
                         channel: "Channel to send the all notes off message to",
                           port: "MIDI port to send to"},

          accepts_block:  false,
          doc:            "Sends a MIDI all notes off message to *all* connected MIDI devices. on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

All devices on a given channel will respond both to data received both over MIDI and Played data, etc. See `midi_local_control_off` off to disable local control.

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_all_notes_off #=> Turn off all notes on MIDI devices on all channels (and ports)",
        "midi_all_notes_off channel: 2 #=> Turn off all notes on MIDI devices on channel 2"
      ]




      def midi_clock_tick(opts={})
        ports = __resolve_midi_ports(opts)
        port  = pp_el_or_list(ports)

        ports.each do |p|
          __midi_send_timed("/#{p}/clock")
        end
        nil
      end
      doc name:           :midi_clock_tick,
          introduced:     Version.new(2,12,0),
          summary:        "Send an individual MIDI clock tick",
          args:           [[]],
          returns:        :nil,
          opts: {
                          channel: "Channel to send the all notes off message to",
                          port: "MIDI port to send to"},

          accepts_block:  false,
          doc:            "Sends a MIDI clock tick message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

Typical MIDI devices expect the clock to send 24 ticks per quarter note (typically a beat). See `midi_clock_beat` for a simple way of sending all the ticks for a given beat.

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_clock_tick #=> Send an individual clock tick to all connected MIDI devices on all ports."
      ]




      def midi_start(opts={})
        ports = __resolve_midi_ports(opts)
        port  = pp_el_or_list(ports)

        ports.each do |p|
          __midi_send_timed("/#{p}/start")
        end
        __delayed_message "midi_start port: #{port}"
      end
      doc name:           :midi_start,
          introduced:     Version.new(2,12,0),
          summary:        "Send MIDI system message - start",
          args:           [[], ],
          returns:        :nil,
          opts:           nil,
          accepts_block:  false,
          doc:            "Sends the MIDI start system message to *all* connected MIDI devices on *all* ports.  Use the `port:` opt to restrict which MIDI ports are used.

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_start #=> Send start message to all connected MIDI devices"
      ]


      def midi_stop(opts={})
        ports = __resolve_midi_ports(opts)
        port  = pp_el_or_list(ports)

        ports.each do |p|
          __midi_send_timed("/#{p}/stop")
        end
        __delayed_message "midi_stop port: #{port}"
      end
      doc name:           :midi_stop,
          introduced:     Version.new(2,12,0),
          summary:        "Send MIDI system message - stop",
          args:           [[]],
          returns:        :nil,
          opts:           {port: "MIDI Port(s) to send the stop message to"},
          accepts_block:  false,
          doc:            "Sends the MIDI stop system message to *all* connected MIDI devices on *all* ports.  Use the `port:` opt to restrict which MIDI ports are used.

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_stop #=> Send stop message to all connected MIDI devices"
      ]

      def midi_continue(opts={})
        ports = __resolve_midi_ports(opts)
        port  = pp_el_or_list(ports)

        ports.each do |p|
          __midi_send_timed("/#{p}/continue")
        end
        __delayed_message "midi_continue port: #{port}"
      end
      doc name:           :midi_continue,
          introduced:     Version.new(2,12,0),
          summary:        "Send MIDI system message - continue",
          args:           [[]],
          returns:        :nil,
          opts:           {port: "MIDI Port(s) to send the continue message to"},
          accepts_block:  false,
          doc:            "Sends the MIDI continue system message to *all* connected MIDI devices on *all* ports.  Use the `port:` opt to restrict which MIDI ports are used.

The continue message continues at the point the sequence was stopped.

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_continue #=> Send continue message to all connected MIDI devices"
      ]



      def midi_clock_beat(dur=1, opts={})
        if dur.is_a?(Hash)
          opts = dur
          dur = 1
        end

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

        ports = __resolve_midi_ports(opts)
        port  = pp_el_or_list(ports)

        ports.each do |p|
          time_warp times do |i, el|
            __midi_send_timed("/#{p}/clock")
          end
        end

        __delayed_message "midi_clock_beat port: #{port}"

      end
      doc name:           :midi_clock_beat,
          introduced:     Version.new(2,12,0),
          summary:        "Send a quarter-note's worth of MIDI clock ticks",
          args:           [[], ],
          returns:        :nil,
          opts:           nil,
          accepts_block:  false,
          doc:            "Sends enough MIDI clock ticks for one beat to *all* connected MIDI devices. Use the `port:` opt to restrict which MIDI ports are used.

Schedules for 24 clock ticks to be sent linearly spread over dur beats.

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi_clock_beat #=> Send 24 clock ticks over a period of 1 beat",
        "midi_clock_beat 0.5 #=> Send 24 clock ticks over a period of 0.5 beat"
      ]





      def midi(*args)
        params, opts = split_params_and_merge_opts_array(args)
        n, vel = *params

        if rest? n
          __delayed_message "midi :rest"
          return nil
        end

        n = normalise_transpose_and_tune_note_from_args(n, opts)

        on_val = opts.fetch(:on, 1)

        on on_val do
          return midi_all_notes_off(opts) if n == :off

          channels = __resolve_midi_channels(opts)
          ports    = __resolve_midi_ports(opts)
          vel      = __resolve_midi_velocity(vel, opts)
          sus      = opts.fetch(:sustain, 1).to_f
          rel_vel  = opts.fetch(:release_velocity, 127)
          n        = n.round.min(0).max(127)
          chan     = pp_el_or_list(channels)
          port     = pp_el_or_list(ports)

          ports.each do |p|
            channels.each do |c|
              __midi_send_timed_pc("/note_on", p, c, [n, vel])
              time_warp sus - 0.01 do
                __midi_send_timed_pc("/note_off", p, c, [n, rel_vel])
              end
            end
          end
          __delayed_message "midi #{n}, #{vel}, sustain: #{sus}, port: #{port}, channel: #{chan}"
        end
        nil
      end
      doc name:           :midi,
          introduced:     Version.new(2,12,0),
          summary:        "Trigger and release an external synth via MIDI",
          args:           [[:note, :number], ],
          returns:        :nil,
          opts:           {sustain: "Duration of note event in beats",
                           vel:  "Velocity of note as a MIDI number"},
          accepts_block:  false,
          doc:            "Sends a MIDI note on event to *All* connected MIDI devices and *all* channels and then after sustain beats sends a MIDI note off event. Ensures MIDI trigger is synchronised with standard calls to play and sample. Co-operates completely with Sonic Pi's timing system including `time_warp`.

If `note` is specified as `:off` then all notes will be turned off (same as `midi_all_notes_off`).

*THIS IS ALPHA!* Expect this fn to completely change before final release",
          examples:       [
        "midi :e1, sustain: 0.3, vel_f: 0.5, channel: 3",
        "midi :off, channel: 3 #=> Turn off all notes on channel 3"
]




      def __resolve_midi_channels(opts)
        channels = (opts[:channel] || opts[:chan] || current_midi_channels)
        if channels == '*'
          return ['*']
        elsif is_list_like?(channels)
          return channels.map do |c|
            return ['*'] if c == '*'
            c.to_i.min(1).max(16)
          end.uniq!
        else
          return [channels.to_i.min(1).max(16)]
        end
      end

      def __resolve_midi_ports(opts)
        ports = (opts[:port]) || current_midi_ports
        if is_list_like?(ports)
          return ['*'] if ports.include?('*')
          return ports
        else
          return [ports]
        end
      end

      def __resolve_midi_velocity(vel, opts)
        if vel = vel || opts[:velocity] || opts[:vel]
          return note(vel).round.min(0).max(127)
        elsif vel = opts[:velocity_f] || opts[:vel_f]
          return (note(vel).to_f * 127).round.min(0).max(127)
        else
          return 127
        end
      end

      def __resolve_midi_val(val, opts)
        if val = val || opts[:value] || opts[:val]
          val = note(val).round.min(0).max(127)
        elsif val = opts[:value_f] || opts[:val_f]
          val = (note(val).to_f * 127).round.min(0).max(127)
        else
          val = 127
        end
      end

      def __midi_send_timed_pc(path, p, c, args)
        c = -1 if c == '*'
        __midi_send_timed "/#{p}#{path}", c, *args
      end

      def __midi_send_timed(*args)
        #TODO remove hardcoded port number
        osmid_o2m_port = 4561
        osc_send "localhost", osmid_o2m_port, *args
      end

    end
  end
end
