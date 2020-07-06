#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2016, 2017 by Sam Aaron (http://sam.aaron.name).
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


      def use_midi_logging(v, &block)
        raise DeprecationError, "use_midi_logging does not work with a do/end block. Perhaps you meant with_midi_logging" if block
        __thread_locals.set(:sonic_pi_suppress_midi_logging, !v)
      end
      doc name:          :use_midi_logging,
          introduced:    Version.new(3,0,0),
          summary:       "Enable and disable MIDI logging",
          doc:           "Enable or disable log messages created on MIDI functions. This does not disable the MIDI functions themselves, it just stops them from being printed to the log",
          args:          [[:true_or_false, :boolean]],
          opts:          nil,
          accepts_block: false,
          examples:      ["use_midi_logging true # Turn on MIDI logging", "use_midi_logging false # Disable MIDI logging"]




      def with_midi_logging(v, &block)
        raise ArgumentError, "with_midi_logging requires a do/end block. Perhaps you meant use_midi_logging" unless block
        current = __thread_locals.get(:sonic_pi_suppress_midi_logging)
        __thread_locals.set(:sonic_pi_suppress_midi_logging, !v)
        block.call
        __thread_locals.set(:sonic_pi_suppress_midi_logging, current)
      end
      doc name:          :with_midi_logging,
          introduced:    Version.new(3,0,0),
          summary:       "Block-level enable and disable MIDI logging",
          doc:           "Similar to use_midi_logging except only applies to code within supplied `do`/`end` block. Previous MIDI log value is restored after block.",
          args:          [[:true_or_false, :boolean]],
          opts:          nil,
          accepts_block: true,
          requires_block: true,
          examples:      ["
  # Turn on MIDI logging:
  use_midi_logging true

  midi :e1 #  message is printed to log

  with_midi_logging false do
    #MIDI logging is now disabled
    midi :f2 # MIDI message *is* sent but not displayed in log
  end
  sleep 1
  # Debug is re-enabled
  midi :G3 # message is displayed in log
  "]



      def use_midi_defaults(*args, &block)
        raise "use_midi_defaults does not work with a block. Perhaps you meant with_midi_defaults" if block
        args_h = resolve_synth_opts_hash_or_array(args)
        args_h.each { |k, v|  v.freeze }
        __thread_locals.set :sonic_pi_mod_midi_defaults, SonicPi::Core::SPMap.new(args_h)
      end
      doc name:          :use_midi_defaults,
          introduced:    Version.new(3,0,0),
          summary:       "Use new MIDI defaults",
          doc:           "Specify new default values to be used by all subsequent calls to `midi_*` fns. Will remove and override any previous defaults.",
          args:          [],
          opts:          {},
          accepts_block: false,
          examples:      ["
midi_note_on :e1 # Sends MIDI :e1 note_on with default opts

use_midi_defaults channel: 3, port: \"foo\"

midi_note_on :e3 # Sends MIDI :e3 note_on to channel 3 on port \"foo\"

use_midi_defaults channel: 1

midi_note_on :e2 # Sends MIDI :e2 note_on to channel 1. Note that the port is back to the default and no longer \"foo\".
"]




      def with_midi_defaults(*args, &block)
        raise "with_midi_defaults must be called with a do/end block" unless block
        current_defs = __thread_locals.get(:sonic_pi_mod_midi_defaults)
        args_h = resolve_synth_opts_hash_or_array(args)
        args_h.each { |k, v|  v.freeze }

        __thread_locals.set :sonic_pi_mod_midi_defaults, SonicPi::Core::SPMap.new(args_h)
        res = block.call
        __thread_locals.set :sonic_pi_mod_midi_defaults, current_defs
        res
      end
      doc name:           :with_midi_defaults,
          introduced:     Version.new(3,0,0),
          summary:        "Block-level use new MIDI defaults",
          doc:            "Specify new default values to be used by all calls to `midi_*` fns within the `do`/`end` block. After the `do`/`end` block has completed the previous MIDI defaults (if any) are restored.",
          args:           [],
          opts:           {},
          accepts_block:  true,
          requires_block: true,
          examples:       ["
midi_note_on :e1 # Sends MIDI :e1 note on with default opts

with_midi_defaults channel: 3, port: \"foo\" do
  midi_note_on :e3 # Sends MIDI :e3 note on to channel 3 on port \"foo\"
end

use_midi_defaults channel: 1   # this will be overridden by the following

with_midi_defaults channel: 5 do
  midi_note_on :e2 # Sends MIDI :e2 note on to channel 5.
                   # Note that the port is back to the default
end

  midi_note_on :e4 # Sends MIDI :e4 note on to channel 1
                   # Note that the call to use_midi_defaults is now honoured.
"]




      def use_merged_midi_defaults(*args, &block)
        raise "use_merged_midi_defaults does not work with a block. Perhaps you meant with_midi_defaults" if block
        current_defs = __thread_locals.get(:sonic_pi_mod_midi_defaults)
        args_h = resolve_synth_opts_hash_or_array(args)
        merged_defs = (current_defs || {}).merge(args_h)
        __thread_locals.set :sonic_pi_mod_midi_defaults, SonicPi::Core::SPMap.new(merged_defs)
      end
      doc name:          :use_merged_midi_defaults,
          introduced:    Version.new(3,0,0),
          summary:       "Merge MIDI defaults",
          doc:           "Specify new default values to be used by all subsequent calls to `midi_*` fns. Merges the specified values with any previous defaults, rather than replacing them",
          args:          [],
          opts:          {},
          accepts_block: false,
          examples:      ["
midi_note_on :e1 # Sends MIDI :e1 note_on with default opts

use_midi_defaults channel: 3, port: \"foo\"

midi_note_on :e3 # Sends MIDI :e3 note_on to channel 3 on port \"foo\"

use_merged_midi_defaults channel: 1

midi_note_on :e2 # Sends MIDI :e2 note_on to channel 1 on port \"foo\".
                 # This is because the call to use_merged_midi_defaults overrode the
                 # channel but not the port which got merged in.
"]




      def with_merged_midi_defaults(*args, &block)
        raise "with_merged_midi_defaults must be called with a do/end block" unless block
        current_defs = __thread_locals.get(:sonic_pi_mod_midi_defaults)

        args_h = resolve_synth_opts_hash_or_array(args)
        merged_defs = (current_defs || {}).merge(args_h)
        __thread_locals.set :sonic_pi_mod_midi_defaults, SonicPi::Core::SPMap.new(merged_defs)
        res = block.call
        __thread_locals.set :sonic_pi_mod_midi_defaults, current_defs
        res
      end
      doc name:           :with_merged_midi_defaults,
          introduced:     Version.new(3,0,0),
          summary:        "Block-level merge midi defaults",
          doc:            "Specify opt values to be used by any following call to the `midi_*` fns within the specified `do`/`end` block. Merges the specified values with any previous midi defaults, rather than replacing them. After the `do`/`end` block has completed, previous defaults (if any) are restored.",
          args:           [],
          opts:           {},
          accepts_block:  true,
          requires_block: true,
          examples:       ["
midi_note_on :e1 # Sends MIDI :e1 note_on with default opts

use_midi_defaults channel: 3, port: \"foo\"

midi_note_on :e3 # Sends MIDI :e3 note_on to channel 3 on port \"foo\"

with_merged_midi_defaults channel: 1 do

  midi_note_on :e2 # Sends MIDI :e2 note_on to channel 1 on port \"foo\".
                   # This is because the call to use_merged_midi_defaults overrode the
                   # channel but not the port which got merged in.
end

midi_note_on :e2 # Sends MIDI :e2 note_on to channel 3 on port \"foo\".
                 # This is because the previous defaults were restored after
                 # the call to with_merged_midi_defaults.

"]




      def current_midi_defaults
        __thread_locals.get(:sonic_pi_mod_midi_defaults) || {}
      end
      doc name:          :current_midi_defaults,
          introduced:    Version.new(3,0,0),
          summary:       "Get current MIDI defaults",
          doc:           "Returns the current MIDI defaults. This is a map of opt names to values

This can be set via the fns `use_midi_defaults`, `with_midi_defaults`, `use_merged_midi_defaults` and `with_merged_midi_defaults`.",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      ["
use_midi_defaults channel: 1, port: \"foo\"
midi_note_on :e1 # Sends MIDI :e1 note on to channel 1 on port \"foo\"
current_midi_defaults #=> Prints {channel: 1, port: \"foo\"}"]




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
            raise "MIDI Port Filter Proc accepts 1 argument only. Found #{block.arity}" unless f.arity == 1
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


      @@number_cache = 0.upto(127).map {|i| i.to_s}

      def midi_note_on(*args)
        params, opts = split_params_and_merge_opts_array(args)
        opts         = current_midi_defaults.merge(opts)
        n, vel = *params

        if rest? n
          __midi_rest_message "midi_note_on :rest"
          return nil
        end

        on_val = opts.fetch(:on, 1)

        if truthy?(on_val)
          n = normalise_transpose_and_tune_note_from_args(n, opts)

          channels = __resolve_midi_channels(opts)
          ports    = __resolve_midi_ports(opts)
          vel      = __resolve_midi_velocity(vel, opts)
          n        = n.round.min(0).max(127)
          chan     = pp_el_or_list(channels)
          port     = pp_el_or_list(ports)

          ports.each do |p|
            channels.each do |c|
              __midi_send_timed_pc("/note_on", p, c, n, vel)
            end
          end
          __midi_message "midi_note_on #{@@number_cache[n]}, #{@@number_cache[vel]}, channel: #{chan}, port: #{port}"
        else
          __midi_rest_message "midi_note_on :rest, on: 0"
        end
        nil
      end
      doc name:           :midi_note_on,
          introduced:     Version.new(3,0,0),
          summary:        "Send MIDI note on message",
          args:           [[:note, :midi], [:velocity, :midi]],
          alt_args:       [[[:note, :midi]]],
          returns:        :nil,
          opts:           {
                           channel: "MIDI channel(s) to send event on",
                           port: "MIDI port(s) to send to",
                           velocity: "Note velocity as a MIDI number.",
                           vel_f: "Velocity as a value between 0 and 1 (will be converted to a MIDI velocity between 0 and 127)",
                           on: "If specified and false/nil/0 will stop the midi note on message from being sent out. (Ensures all opts are evaluated in this call to `midi_note_on` regardless of value)."},
          accepts_block:  false,
          doc:            "Sends a MIDI Note On Event to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to indepently restrict which MIDI ports and channels are used.

Note and velocity values can be passed as a note symbol such as `:e3` or a MIDI number such as 52. Decimal values will be rounded down or up to the nearest whole number - so values between 3.5 and 4 will be rounded up to 4 and values between 3.49999... and 3 will be rounded down to 3. These values will also be clipped within the range 0->127 so all values lower than 0 will be increased to 0 and all values greater than 127 will be reduced to 127.

The `velocity` param may be omitted - in which case it will default to 127 unless you supply it as an opt via the keys `velocity:` or `vel_f:`.

You may also optionally pass the velocity value as a floating point value between 0 and 1 such as 0.2 or 0.785 (which will be linearly mapped to MIDI values between 0 and 127) using the vel_f: opt.

[MIDI 1.0 Specification - Channel Voice Messages - Note on event](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)
",
      examples:       [
        "midi_note_on :e3  #=> Sends MIDI note on :e3 with the default velocity of 12 to all ports and channels",
        "midi_note_on :e3, 12  #=> Sends MIDI note on :e3 with velocity 12 to all channels",
        "midi_note_on :e3, 12, channel: 3  #=> Sends MIDI note on :e3 with velocity 12 on channel 3",
        "midi_note_on :e3, velocity: 100 #=> Sends MIDI note on for :e3 with velocity 100",
        "midi_note_on :e3, vel_f: 0.8 #=> Scales velocity 0.8 to MIDI value 102 and sends MIDI note on for :e3 with velocity 102",
        "midi_note_on 60.3, 50.5 #=> Rounds params up or down to the nearest whole number and sends MIDI note on for note 60 with velocity 51",
        "midi_note_on :e3, channel: [1, 3, 5] #=> Send MIDI note :e3 on to channels 1, 3, 5 on all connected ports",
        "midi_note_on :e3, port: [\"foo\", \"bar\"] #=> Send MIDI note :e3 on to on all channels on ports named \"foo\" and \"bar\"",
        "midi_note_on :e3, channel: 1, port: \"foo\" #=> Send MIDI note :e3 on only on channel 1 on port \"foo\""
      ]




      def midi_note_off(*args)
        params, opts = split_params_and_merge_opts_array(args)
        opts         = current_midi_defaults.merge(opts)
        n, vel = *params

        if rest? n
          __midi_rest_message "midi_note_off :rest"
          return nil
        end

        n = normalise_transpose_and_tune_note_from_args(n, opts)

        on_val = opts.fetch(:on, 1)

        if truthy?(on_val)
          channels = __resolve_midi_channels(opts)
          ports    = __resolve_midi_ports(opts)
          vel      = __resolve_midi_velocity(vel, opts)
          n        = note(n).round.min(0).max(127)
          chan     = pp_el_or_list(channels)
          port     = pp_el_or_list(ports)

          ports.each do |p|
            channels.each do |c|
              __midi_send_timed_pc("/note_off", p, c, n, vel)
            end
          end
          __midi_message "midi_note_off #{@@number_cache[n]}, #{@@number_cache[vel]}, channel: #{chan}, port: #{port}"
        else
          __midi_rest_message "midi_note_off :rest, on: 0"
        end
        nil
      end
      doc name:           :midi_note_off,
          introduced:     Version.new(3,0,0),
          summary:        "Send MIDI note off message",
          args:           [[:note, :midi], [:release_velocity, :midi]],
          alt_args:       [[[:note, :midi]]],
          returns:        :nil,
          opts:           {
                           channel: "MIDI channel(s) to send event on as a number or list of numbers.",
                           port: "MIDI port(s) to send to as a string or list of strings.",
                           velocity: "Release velocity as a MIDI number.",
                           vel_f: "Release velocity as a value between 0 and 1 (will be converted to a MIDI velocity)",
                           on: "If specified and false/nil/0 will stop the midi note off message from being sent out. (Ensures all opts are evaluated in this call to `midi_note_off` regardless of value)."},
          accepts_block:  false,
          doc:            "Sends the MIDI note off message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

Note and release velocity values can be passed as a note symbol such as `:e3` or a number. Decimal values will be rounded down or up to the nearest whole number - so values between 3.5 and 4 will be rounded up to 4 and values between 3.49999... and 3 will be rounded down to 3. These values will also be clipped within the range 0->127 so all values lower then 0 will be increased to 0 and all values greater than 127 will be reduced to 127.

The `release_velocity` param may be omitted - in which case it will default to 127 unless you supply it as a named opt via the keys `velocity:` or `vel_f:`.

You may also optionally pass the release velocity value as a floating point value between 0 and 1 such as 0.2 or 0.785 (which will be mapped to MIDI values between 0 and 127) using the `vel_f:` opt.

[MIDI 1.0 Specification - Channel Voice Messages - Note off event](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)
",
      examples:       [
        "midi_note_off :e3 #=> Sends MIDI note off for :e3 with the default release velocity of 127 to all ports and channels",
        "midi_note_off :e3, 12  #=> Sends MIDI note off on :e3 with velocity 12 on all channels",
        "midi_note_off :e3, 12, channel: 3  #=> Sends MIDI note off on :e3 with velocity 12 to channel 3",
        "midi_note_off :e3, velocity: 100 #=> Sends MIDI note on for :e3 with release velocity 100",
        "midi_note_off :e3, vel_f: 0.8 #=> Scales release velocity 0.8 to MIDI value 102 and sends MIDI note off for :e3 with release velocity 102",
        "midi_note_off 60.3, 50.5 #=> Rounds params up or down to the nearest whole number and sends MIDI note off for note 60 with velocity 51",
        "midi_note_off :e3, channel: [1, 3, 5] #=> Send MIDI note off on :e3 to channels 1, 3, 5 on all connected ports",
        "midi_note_off :e3, port: [\"foo\", \"bar\"] #=> Send MIDI note off on :e3 to on all channels on ports named \"foo\" and \"bar\"",
        "midi_note_off :e3, channel: 1, port: \"foo\" #=> Send MIDI note off on :e3 only on channel 1 on port \"foo\""
      ]




      def midi_poly_pressure(*args)
        params, opts     = split_params_and_merge_opts_array(args)
        opts             = current_midi_defaults.merge(opts)
        control_num, val = *params

        if rest? control_num
          __midi_message "midi_poly_pressure :rest"
          return nil
        end

        on_val = opts.fetch(:on, 1)

        if truthy?(on_val)
          channels    = __resolve_midi_channels(opts)
          ports       = __resolve_midi_ports(opts)
          val         = __resolve_midi_val(val, opts)
          control_num = note(control_num).round.min(0).max(127)
          chan        = pp_el_or_list(channels)
          port        = pp_el_or_list(ports)

          ports.each do |p|
            channels.each do |c|
              __midi_send_timed_pc("/poly_pressure", p, c, control_num, val)
            end
          end
          __midi_message "midi_poly_pressure #{control_num}, #{val}, port: #{port}, channel: #{chan}"
        else
          __midi_rest_message "midi_poly_pressure :rest, on: 0"
        end
        nil
      end
      doc name:           :midi_poly_pressure,
          introduced:     Version.new(3,0,0),
          summary:        "Send a MIDI polyphonic key pressure message",
          args:           [[:note, :midi], [:value, :midi]],
          returns:        :nil,
          opts:           {
                           channel: "Channel(s) to send to",
                           port: "MIDI port(s) to send to",
                           value: "Pressure value as a MIDI number.",
                           val_f: "Pressure value as a value between 0 and 1 (will be converted to a MIDI value)",
                           on: "If specified and false/nil/0 will stop the midi poly pressure message from being sent out. (Ensures all opts are evaluated in this call to `midi_poly_pressure` regardless of value)."},
          accepts_block:  false,
          doc:            "Sends a MIDI polyphonic key pressure message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

Note number and pressure value can be passed as a note such as `:e3` and decimal values will be rounded down or up to the nearest whole number - so values between 3.5 and 4 will be rounded up to 4 and values between 3.49999... and 3 will be rounded down to 3.

You may also optionally pass the pressure value as a floating point value between 0 and 1 such as 0.2 or 0.785 (which will be mapped to MIDI values between 0 and 127) using the `val_f:` opt.

[MIDI 1.0 Specification - Channel Voice Messages - Polyphonic Key Pressure (Aftertouch)](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)
",
          examples:       [
        "midi_poly_pressure 100, 32  #=> Sends a MIDI poly key pressure message to control note 100 with value 32 to all ports and channels",
        "midi_poly_pressure :e7, 32  #=> Sends a MIDI poly key pressure message to control note 100 with value 32 to all ports and channels",
        "midi_poly_pressure 100, 32, channel: 5  #=> Sends MIDI poly key pressure message to control note 100 with value 32 on channel 5 to all ports",
        "midi_poly_pressure 100, val_f: 0.8, channel: 5  #=> Sends a MIDI poly key pressure message to control note 100 with value 102 on channel 5 to all ports",
        "midi_poly_pressure 100, value: 102, channel: [1, 5]  #=> Sends MIDI poly key pressure message to control note 100 with value 102 on channel 1 and 5 to all ports"
      ]




      def midi_cc(*args)
        params, opts     = split_params_and_merge_opts_array(args)
        opts             = current_midi_defaults.merge(opts)
        control_num, val = *params

        if rest? control_num
          __midi_message "midi_cc :rest"
          return nil
        end

        on_val = opts.fetch(:on, 1)

        if truthy?(on_val)
          channels    = __resolve_midi_channels(opts)
          ports       = __resolve_midi_ports(opts)
          val         = __resolve_midi_val(val, opts)
          control_num = note(control_num).round.min(0).max(127)
          chan        = pp_el_or_list(channels)
          port        = pp_el_or_list(ports)

          ports.each do |p|
            channels.each do |c|
              __midi_send_timed_pc("/control_change", p, c, control_num, val)
            end
          end
          __midi_message "midi_cc #{control_num}, #{val}, port: #{port}, channel: #{chan}"
        else
          __midi_rest_message "midi_cc :rest, on: 0"
        end
        nil
      end
      doc name:           :midi_cc,
          introduced:     Version.new(3,0,0),
          summary:        "Send MIDI control change message",
          args:           [[:control_num, :midi], [:value, :midi]],
          returns:        :nil,
          opts:           {
                           channel: "Channel(s) to send to",
                           port: "MIDI port(s) to send to",
                           value: "Control value as a MIDI number.",
                           val_f: "Control value as a value between 0 and 1 (will be converted to a MIDI value)",
                           on: "If specified and false/nil/0 will stop the midi cc message from being sent out. (Ensures all opts are evaluated in this call to `midi_cc` regardless of value)."},
          accepts_block:  false,
          doc:            "Sends a MIDI control change message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

Control number and control value can be passed as a note such as `:e3` and decimal values will be rounded down or up to the nearest whole number - so values between 3.5 and 4 will be rounded up to 4 and values between 3.49999... and 3 will be rounded down to 3.

You may also optionally pass the control value as a floating point value between 0 and 1 such as 0.2 or 0.785 (which will be mapped to MIDI values between 0 and 127) using the `val_f:` opt.

[MIDI 1.0 Specification - Channel Voice Messages - Control change](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)
",
          examples:       [
        "midi_cc 100, 32  #=> Sends MIDI cc message to control 100 with value 32 to all ports and channels",
        "midi_cc :e7, 32  #=> Sends MIDI cc message to control 100 with value 32 to all ports and channels",
        "midi_cc 100, 32, channel: 5  #=> Sends MIDI cc message to control 100 with value 32 on channel 5 to all ports",
        "midi_cc 100, val_f: 0.8, channel: 5  #=> Sends MIDI cc message to control 100 with value 102 on channel 5 to all ports",
        "midi_cc 100, value: 102, channel: [1, 5]  #=> Sends MIDI cc message to control 100 with value 102 on channel 1 and 5 to all ports"
]


      def midi_channel_pressure(*args)
        params, opts = split_params_and_merge_opts_array(args)
        opts         = current_midi_defaults.merge(opts)
        pressure     = params[0]

        if params.size > 0 && rest?(pressure)
          __midi_message "midi_channel_pressure :rest"
          return nil
        end

        on_val = opts.fetch(:on, 1)

        if truthy?(on_val)
          channels = __resolve_midi_channels(opts)
          ports    = __resolve_midi_ports(opts)
          pressure = __resolve_midi_val(pressure, opts)
          chan     = pp_el_or_list(channels)
          port     = pp_el_or_list(ports)

          ports.each do |p|
            channels.each do |c|
              __midi_send_timed_pc("/channel_pressure", p, c, pressure)
            end
          end
          __midi_message "midi_channel_pressure #{pressure}, port: #{port}, channel: #{chan}"
        else
          __midi_rest_message "midi_channel_pressure :rest, on: 0"
        end
        nil
      end
      doc name:           :midi_channel_pressure,
          introduced:     Version.new(3,0,0),
          summary:        "Send MIDI channel pressure (aftertouch) message",
          args:           [[:val, :midi]],
          returns:        :nil,
          opts:           {
                           channel: "Channel(s) to send to",
                           port: "MIDI port(s) to send to",
                           value: "Pressure value as a MIDI number.",
                           val_f: "Pressure value as a value between 0 and 1 (will be converted to a MIDI value)",
                           on: "If specified and false/nil/0 will stop the midi channel pressure message from being sent out. (Ensures all opts are evaluated in this call to `midi_channel_pressure` regardless of value)."},
          accepts_block:  false,
          doc:            "Sends a MIDI channel pressure (aftertouch) message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

The pressure value can be passed as a note such as `:e3` and decimal values will be rounded down or up to the nearest whole number - so values between 3.5 and 4 will be rounded up to 4 and values between 3.49999... and 3 will be rounded down to 3.

You may also optionally pass the pressure value as a floating point value between 0 and 1 such as 0.2 or 0.785 (which will be mapped to MIDI values between 0 and 127) using the `val_f:` opt.

[MIDI 1.0 Specification - Channel Voice Messages - Channel Pressure (Aftertouch)](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)
",
          examples:       [
        "midi_channel_pressure 50  #=> Sends MIDI channel pressure message with value 50 to all ports and channels",
        "midi_channel_pressure :C4  #=> Sends MIDI channel pressure message with value 60 to all ports and channels",
        "midi_channel_pressure 0.5  #=> Sends MIDI channel pressure message with value 63.5 to all ports and channels",
        "midi_channel_pressure 30, channel: [1, 5]  #=> Sends MIDI channel pressure message with value 30 on channel 1 and 5 to all ports"
]




      def midi_pitch_bend(*args)
        params, opts = split_params_and_merge_opts_array(args)
        opts         = current_midi_defaults.merge(opts)
        delta        = params[0]

        if params.size > 0 && rest?(delta)
          __midi_message "midi_pitch_bend :rest"
          return nil
        end

        on_val = opts.fetch(:on, 1)

        if truthy?(on_val)
          channels           = __resolve_midi_channels(opts)
          ports              = __resolve_midi_ports(opts)
          delta, delta_midi  = __resolve_midi_deltas(delta, opts)
          chan               = pp_el_or_list(channels)
          port               = pp_el_or_list(ports)

          ports.each do |p|
            channels.each do |c|
              __midi_send_timed_pc("/pitch_bend", p, c, delta_midi)
            end
          end
          __midi_message "midi_pitch_bend #{delta}, delta_midi: #{delta_midi}, port: #{port}, channel: #{chan}"
        else
          __midi_rest_message "midi_pitch_bend :rest, on: 0"
        end
        nil
      end
      doc name:           :midi_pitch_bend,
          introduced:     Version.new(3,0,0),
          summary:        "Send MIDI pitch bend message",
          args:           [[:delta, :float01]],
          returns:        :nil,
          opts:           {
                           channel: "Channel(s) to send to",
                           port: "MIDI port(s) to send to",
                           delta: "Pitch bend value as a number between 0 and 1 (will be converted to a value between 0 and 16383). No bend is the central value 0.5",
                           delta_midi: "Pitch bend value as a number between 0 and 16383 inclusively. No bend is central value 8192.",

                           on: "If specified and false/nil/0 will stop the midi pitch bend message from being sent out. (Ensures all opts are evaluated in this call to `midi_pitch_bend` regardless of value)."},
          accepts_block:  false,
          doc:            "Sends a MIDI pitch bend message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

Delta value is between 0 and 1 with 0.5 representing no pitch bend, 1 max pitch bend and 0 minimum pitch bend.

Typical MIDI values such as note or cc are represented with 7 bit numbers which translates to the range 0-127. This makes sense for keyboards which have at most 88 keys. However, it translates to a poor resolution when working with pitch bend. Therefore, pitch bend is unlike most MIDI values in that it has a much greater range: 0 - 16383 (by virtue of being represented by 14 bits).

* It is also possible to specify the delta value as a (14 bit) MIDI pitch bend value between 0 and 16383 using the `delta_midi:` opt.
* When using the `delta_midi:` opt no pitch bend is the value 8192

[MIDI 1.0 Specification - Channel Voice Messages - Pitch Bend Change](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)
",
          examples:       [
        "midi_pitch_bend 0  #=> Sends MIDI pitch bend message with value 0 to all ports and channels",
        "midi_pitch_bend 1  #=> Sends MIDI pitch bend message with value 16383 to all ports and channels",
        "midi_pitch_bend 0.5  #=> Sends MIDI pitch bend message with value 8192 to all ports and channels",
        "midi_pitch_bend delta_midi: 8192  #=> Sends MIDI pitch bend message with value 8192 to all ports and channels",
        "midi_pitch_bend 0, channel: [1, 5]  #=> Sends MIDI pitch bend message with value 0 on channel 1 and 5 to all ports"
]

      def midi_pc(*args)
        params, opts    = split_params_and_merge_opts_array(args)
        opts            = current_midi_defaults.merge(opts)
        program_num     = params[0]
        ports           = __resolve_midi_ports(opts)
        on_val          = opts.fetch(:on, 1)

        if program_num == nil #deal with missing midi_pc paramter
          return nil
        end

        program_num = note(program_num).round.min(0).max(127)

        if truthy?(on_val)
          channels       = __resolve_midi_channels(opts)
          ports          = __resolve_midi_ports(opts)
          chan           = pp_el_or_list(channels)
          port           = pp_el_or_list(ports)

          ports.each do |p|
            channels.each do |c|
              __midi_send_timed_pc("/program_change", p, c, program_num)
            end
          end
          __midi_message "midi_pc #{program_num}, port: #{port}, channel: #{chan}"
        else
          __midi_message "midi_pc  #{program_num}, on: 0"
        end
        nil
      end
      doc name:           :midi_pc,
          introduced:     Version.new(3,0,2),
          summary:        "Send MIDI program change message",
          args:           [[:program_num, :midi]],
          returns:        :nil,
          opts:           {
                           channel: "Channel(s) to send to",
                           port: "MIDI port(s) to send to",
                           on: "If specified and false/nil/0 will stop the midi pc message from being sent out. (Ensures all opts are evaluated in this call to `midi_pc` regardless of value)."},
          accepts_block:  false,
          doc:            "Sends a MIDI program change message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

Program number can be passed as a note such as `:e3` and decimal values will be rounded down or up to the nearest whole number - so values between 3.5 and 4 will be rounded up to 4 and values between 3.49999... and 3 will be rounded down to 3.

[MIDI 1.0 Specification - Channel Voice Messages - Program change](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)
",
          examples:       [
        "midi_pc 100  #=> Sends MIDI pc message to all ports and channels",
        "midi_pc :e7  #=> Sends MIDI pc message to all ports and channels",
        "midi_pc 100, channel: 5  #=> Sends MIDI pc message on channel 5 to all ports",
        "midi_pc 100, channel: 5  #=> Sends MIDI pc message on channel 5 to all ports",
        "midi_pc 100, channel: [1, 5]  #=> Sends MIDI pc message on channel 1 and 5 to all ports"
]


      def midi_raw(*args)
        params, opts = split_params_and_merge_opts_array(args)
        opts         = current_midi_defaults.merge(opts)
        a, b, c      = params
        a            = a.to_f.round
        b            = b.to_f.round
        c            = c.to_f.round
        ports        = __resolve_midi_ports(opts)
        on_val       = opts.fetch(:on, 1)

        if truthy?(on_val)
          ports.each do |p|
            __midi_send_timed_param_3("/#{p}/raw", a, b, c)
          end
          port = pp_el_or_list(ports)
          __midi_message "midi_raw #{a}, #{b}, #{c}, port: #{port}"
        else
          __midi_message "midi_raw #{a}, #{b}, #{c}, on: 0"
        end
        nil
      end
      doc name:           :midi_raw,
          introduced:     Version.new(3,0,0),
          summary:        "Send raw MIDI message",
          args:           [[:a, :byte], [:b, :byte], [:c, :byte]],
          returns:        :nil,
          opts:           {port: "Port(s) to send the raw MIDI message events to",
                           on: "If specified and false/nil/0 will stop the raw midi message from being sent out. (Ensures all opts are evaluated in this call to `midi_raw` regardless of value)."},
          accepts_block:  false,
          doc:            "Sends the raw MIDI message to *all* connected MIDI devices. Gives you direct access to the individual bytes of a MIDI message. Typically this should be rarely used - prefer the other `midi_` fns where possible.

A raw MIDI message consists of 3 separate bytes - the Status Byte and two Data Bytes. These may be passed as base 10 decimal integers between 0 and 255, in hex form by prefixing `0x` such as `0xb0` which in decimal is 176 or binary form by prefixing `0b` such as `0b01111001` which represents 121 in decimal.

Floats will be rounded up or down to the nearest whole number e.g. 176.1 -> 176, 120.5 -> 121, 0.49 -> 0.

Non-number values will be automatically turned into numbers prior to sending the event if possible (if this conversion does not work an Error will be thrown).

See https://www.midi.org/specifications/item/table-1-summary-of-midi-message for a summary of MIDI messages and their corresponding byte structures.
",
          examples: [
        "midi_raw 176, 121, 0  #=> Sends the MIDI reset command",
        "midi_raw 176.1, 120.5, 0.49  #=> Sends the MIDI reset command (values are rounded down, up and down respectively)",
        "midi_raw 0xb0, 0x79, 0x0  #=> Sends the MIDI reset command",
        "midi_raw 0b10110000, 0b01111001, 0b00000000  #=> Sends the MIDI reset command"
]


      def midi_sysex(*args)
        params, opts = split_params_and_merge_opts_array(args)
        params       = params.map { |p| p.to_f.round }
        opts         = current_midi_defaults.merge(opts)
        ports        = __resolve_midi_ports(opts)
        on_val       = opts.fetch(:on, 1)

        raise "sysex messages must be at least 3 bytes long" if params.length < 3
        raise "sysex messages must start with 0xf0" unless params[0] == 0xf0
        raise "sysex messages must end with 0xf7" unless params[-1] == 0xf7

        if truthy?(on_val)
          ports.each do |p|
            __midi_send_timed_param_n("/#{p}/raw", *params)
          end
          port = pp_el_or_list(ports)
          __midi_message "midi_sysex #{params * ', '}, port: #{port}"
        else
          __midi_message "midi_sysex #{params * ', '}, on: 0"
        end
        nil
      end
      doc name:           :midi_sysex,
          introduced:     Version.new(3,2,0),
          summary:        "Send MIDI System Exclusive (SysEx) message",
          args:           [],
          returns:        :nil,
          opts:           {port: "Port(s) to send the MIDI SysEx message events to",
                           on: "If specified and false/nil/0 will stop the midi SysEx message from being sent out. (Ensures all opts are evaluated in this call to `midi_sysex` regardless of value)."},
          accepts_block:  false,
          doc:            "Sends the MIDI SysEx message to *all* connected MIDI devices.

MIDI SysEx messages, unlike all other MIDI messages, are variable in length. They allow MIDI device manufacturers to define device-specific messages, for example loading/saving patches, or programming device features such as illuminated buttons.

Floats will be rounded up or down to the nearest whole number e.g. 176.1 -> 176, 120.5 -> 121, 0.49 -> 0.

Non-number values will be automatically turned into numbers prior to sending the event if possible (if this conversion does not work an Error will be thrown).
",
          examples: [
        "midi_sysex 0xf0, 0x00, 0x20, 0x6b, 0x7f, 0x42, 0x02, 0x00, 0x10, 0x77, 0x11, 0xf7  #=> Program an Arturia Beatstep controller to turn the eighth pad pink"
]


      def midi_sound_off(*args)
        params, opts = split_params_and_merge_opts_array(args)
        opts         = current_midi_defaults.merge(opts)
        on_val       = opts.fetch(:on, 1)

        ports    = __resolve_midi_ports(opts)
        channels = __resolve_midi_channels(opts)
        port     = pp_el_or_list(ports)
        chan     = pp_el_or_list(channels)

        if truthy?(on_val)
          ports.each do |p|
            channels.each do |c|
              __midi_send_timed_pc("/control_change", p, c, 120, 0)
            end
          end
          __midi_message "midi_sound_off port: #{port}, channel: #{chan}"
        else
          __midi_rest_message "midi_sound_off  port: #{port}, channel: #{chan}, on: 0"
        end
        nil
      end
      doc name:           :midi_sound_off,
          introduced:     Version.new(3,0,0),
          summary:        "Silence all MIDI devices",
          args:           [],
          returns:        :nil,
          opts: {
                          channel: "Channel to send the sound off message to",
                          port: "MIDI port to send to",
                          on: "If specified and false/nil/0 will stop the midi sound off on message from being sent out. (Ensures all opts are evaluated in this call to `midi_sound_off` regardless of value)."},
          accepts_block:  false,
          doc:            "Sends a MIDI sound off message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

All oscillators will turn off, and their volume envelopes are set to zero as soon as possible.

[MIDI 1.0 Specification - Channel Mode Messages - All Sound Off](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)
",
          examples:       [
        "midi_sound_off #=> Silence MIDI devices on all ports and channels",
        "midi_sound_off channel: 2 #=> Silence MIDI devices on channel 2"
      ]




      def midi_reset(*args)
        __info "Resetting MIDI Subsystems"
        params, opts = split_params_and_merge_opts_array(args)
        opts         = current_midi_defaults.merge(opts)
        reset_val    = opts[:value] || opts[:val] || params[0] || 0
        on_val       = opts.fetch(:on, 1)

        ports    = __resolve_midi_ports(opts)
        channels = __resolve_midi_channels(opts)
        port     = pp_el_or_list(ports)
        chan     = pp_el_or_list(channels)

        if truthy?(on_val)
          ports.each do |p|
            channels.each do |c|
              __midi_send_timed_pc("/control_change", p, c, 121, reset_val)
            end
          end
          __midi_message "midi_reset port: #{port}, channel: #{chan}"
        else
          __midi_rest_message "midi_reset port: #{port}, channel: #{chan}, on: 0"
        end
        nil
      end
      doc name:           :midi_reset,
          introduced:     Version.new(3,0,0),
          summary:        "Reset MIDI devices",
          args:           [[:value, :number]],
          returns:        :nil,
          opts: {
                          channel: "Channel to send the midi reset message to",
                          port: "MIDI port to send to",
                          value: "Value must only be zero (the default) unless otherwise allowed in a specific Recommended Practice",
                          on: "If specified and false/nil/0 will stop the midi reset message from being sent out. (Ensures all opts are evaluated in this call to `midi_reset` regardless of value)."},

          accepts_block:  false,
          doc:            "Sends a MIDI reset all controllers message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

All controller values are reset to their defaults.

[MIDI 1.0 Specification - Channel Mode Messages - Reset All Controllers](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)
",
      examples:       [
        "midi_reset #=> Reset MIDI devices on all channels (and ports)",
        "midi_reset channel: 2 #=> Reset MIDI devices on channel 2"
      ]




      def midi_local_control_off(*args)
        params, opts = split_params_and_merge_opts_array(args)
        opts         = current_midi_defaults.merge(opts)
        on_val       = opts.fetch(:on, 1)

        ports    = __resolve_midi_ports(opts)
        channels = __resolve_midi_channels(opts)
        port     = pp_el_or_list(ports)
        chan     = pp_el_or_list(channels)

        if truthy?(on_val)
          ports.each do |p|
            channels.each do |c|
              __midi_send_timed_pc("/control_change", p, c, 122, 0)
            end
          end
          __midi_message "midi_mode_local_control_off port: #{port}, channel: #{chan}"
        else
          __midi_rest_message "midi_mode_local_control_off port: #{port}, channel: #{chan}, on: 0"
        end

        nil
      end
      doc name:           :midi_local_control_off,
          introduced:     Version.new(3,0,0),
          summary:        "Disable local control on MIDI devices",
          args:           [],
          returns:        :nil,
          opts: {
                   channel: "Channel to send the local control off message to",
                   port: "MIDI port to send to",
                   on: "If specified and false/nil/0 will stop the midi local control off message from being sent out. (Ensures all opts are evaluated in this call to `midi_local_control_off` regardless of value)."},
          accepts_block:  false,
          doc:            "Sends a MIDI local control off message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

All devices on a given channel will respond only to data received over MIDI. Played data, etc. will be ignored. See `midi_local_control_on` to enable local control.

[MIDI 1.0 Specification - Channel Mode Messages - Local Control Off](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)
",
          examples:       [
        "midi_local_control_off #=> Disable local control on MIDI devices on all channels (and ports)",
        "midi_local_control_off channel: 2 #=> Disable local control on MIDI devices on channel 2"
      ]




      def midi_local_control_on(*args)
        params, opts = split_params_and_merge_opts_array(args)
        opts         = current_midi_defaults.merge(opts)
        on_val       = opts.fetch(:on, 1)

        ports    = __resolve_midi_ports(opts)
        channels = __resolve_midi_channels(opts)
        port     = pp_el_or_list(ports)
        chan     = pp_el_or_list(channels)

        if truthy?(on_val)
          ports.each do |p|
            channels.each do |c|
              __midi_send_timed_pc("/control_change", p, c, 122, 127)
            end
          end
          __midi_message "midi_mode_local_control_on port: #{port}, channel: #{chan}"
        else
          __midi_rest_message "midi_mode_local_control_on port: #{port}, channel: #{chan}, on: 0"
        end
        nil
      end
      doc name:           :midi_local_control_on,
          introduced:     Version.new(3,0,0),
          summary:        "Enable local control on MIDI devices",
          args:           [],
          returns:        :nil,
          opts: {
                   channel: "Channel to send the local control on message to",
                   port: "MIDI port to send to",
                   on: "If specified and false/nil/0 will stop the midi local control on message from being sent out. (Ensures all opts are evaluated in this call to `midi_local_control_on` regardless of value)."},

          accepts_block:  false,
          doc:            "Sends a MIDI local control on message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

All devices on a given channel will respond both to data received over MIDI and played data, etc. See `midi_local_control_off` to disable local control.

[MIDI 1.0 Specification - Channel Mode Messages - Local Control On](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)
",
          examples:       [
        "midi_local_control_on #=> Enable local control on MIDI devices on all channels (and ports)",
        "midi_local_control_on channel: 2 #=> Enable local control on MIDI devices on channel 2"
      ]




      def midi_mode(*args)
        params, opts = split_params_and_merge_opts_array(args)
        opts         = current_midi_defaults.merge(opts)
        on_val       = opts.fetch(:on, 1)
        mode         = opts[:mode] || params[0] || :omni_off
        channels     = __resolve_midi_channels(opts)
        ports        = __resolve_midi_ports(opts)
        port         = pp_el_or_list(ports)
        chan         = pp_el_or_list(channels)

        case mode
        when :omni_off
          if truthy?(on_val)
            ports.each do |p|
              channels.each do |c|
                __midi_send_timed_pc("/control_change", p, c, 124, 0)
              end
            end
            __midi_message "midi_mode :omni_off, port: #{port}, channel: #{chan}"
          else
            __midi_rest_message "midi_mode :omni_off, port: #{port}, channel: #{chan}, on: 0"
          end
        when :omni_on
          if truthy?(on_val)
            ports.each do |p|
              channels.each do |c|
                __midi_send_timed_pc("/control_change", p, c, 125, 0)
              end
            end
            __midi_message "midi_mode :omni_on, port: #{port}, channel: #{chan}"
          else
            __midi_rest_message "midi_mode :omni_on, port: #{port}, channel: #{chan}, on: 0"
          end
        when :mono
          num_chans = opts[:num_chans] || 16
          if truthy?(on_val)
            ports.each do |p|
              channels.each do |c|
                __midi_send_timed_pc("/control_change", p, c, 126, num_chans)
              end
            end
            __midi_message "midi_mode :mono, num_chans: #{num_chans}, port: #{port}, channel: #{chan}"
          else
            __midi_rest_message "midi_mode :mono, num_chans: #{num_chans}, port: #{port}, channel: #{chan}, on: 0"
          end
        when :poly
          if truthy?(on_val)
            ports.each do |p|
              channels.each do |c|
                __midi_send_timed_pc("/control_change", p, c, 127, 0)
              end
            end
            __midi_message "midi_mode :poly, port: #{port}, channel: #{chan}"
          else
            __midi_rest_message "midi_mode :poly, port: #{port}, channel: #{chan}, on: 0"
          end
        else
          raise "Unknown special mode for midi_mode: #{mode.inspect}. Expected one of: :omni_off, :omni_on, :mono or :poly."
        end
        nil
      end
      doc name:           :midi_mode,
          introduced:     Version.new(3,0,0),
          summary:        "Set Omni/Mono/Poly mode",
          args:           [[:mode, :mode_keyword]],
          returns:        :nil,
          opts: {
                         channel: "Channel to send the MIDI mode message to",
                         port: "MIDI port to send to",
                         mode: "Mode keyword - one of :omni_off, :omni_on, :mono or :poly",
                         num_chans: "Used in mono mode only - Number of channels (defaults to 16)",
                         on: "If specified and false/nil/0 will stop the midi local control off message from being sent out. (Ensures all opts are evaluated in this call to `midi_local_control_off` regardless of value)."},

          accepts_block:  false,
          doc:            "Sends the Omni/Mono/Poly MIDI mode message to *all* connected MIDI devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

Valid modes are:

:omni_off - Omni Mode Off
:omni_on  - Omni Mode On
:mono     - Mono Mode On (Poly Off). Set num_chans: to be the number of channels to use (Omni Off) or 0 (Omni On). Default for num_chans: is 16.
:poly     - Poly Mode On (Mono Off)

Note that this fn also includes the behaviour of `midi_all_notes_off`.

[MIDI 1.0 Specification - Channel Mode Messages - Omni Mode Off | Omni Mode On | Mono Mode On (Poly Off) | Poly Mode On](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)
",
          examples:       [
        "midi_mode :omni_on #=> Turn Omni Mode On on all ports and channels",
        "midi_mode :mono, num_chans: 5 #=> Mono Mode On, Omni off using 5 channels.",
        "midi_mode :mono, num_chans: 0 #=> Mono Mode On, Omni on.",
        "midi_mode :mono #=> Mono Mode On, Omni off using 16 channels (the default) ."
      ]




      def midi_all_notes_off(*args)
        params, opts = split_params_and_merge_opts_array(args)
        opts         = current_midi_defaults.merge(opts)
        on_val       = opts.fetch(:on, 1)

        channels = __resolve_midi_channels(opts)
        ports    = __resolve_midi_ports(opts)
        port     = pp_el_or_list(ports)
        chan     = pp_el_or_list(channels)

        if truthy?(on_val)
          ports.each do |p|
            channels.each do |c|
              __midi_send_timed_pc("/control_change", p, c, 123, 0)
            end
          end
          __midi_message "midi_all_notes_off port: #{port}, channel: #{chan}"
        else
          __midi_rest_message "midi_all_notes_off port: #{port}, channel: #{chan}, on: 0"
        end
        nil
      end
      doc name:           :midi_all_notes_off,
          introduced:     Version.new(3,0,0),
          summary:        "Turn off all notes on MIDI devices",
          args:           [],
          returns:        :nil,
          opts: {
                          channel: "Channel to send the all notes off message to",
                          port: "MIDI port to send to",
                          on: "If specified and false/nil/0 will stop the midi all notes off message from being sent out. (Ensures all opts are evaluated in this call to `midi_all_notes_off` regardless of value)."},
          accepts_block:  false,
          doc:            "Sends a MIDI all notes off message to *all* connected MIDI devices. on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

When an All Notes Off event is received, all oscillators will turn off.

[MIDI 1.0 Specification - Channel Mode Messages - All Notes Off](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)
",
          examples:       [
        "midi_all_notes_off #=> Turn off all notes on MIDI devices on all channels (and ports)",
        "midi_all_notes_off channel: 2 #=> Turn off all notes on MIDI devices on channel 2"
      ]




      def midi_clock_tick(*args)
        params, opts = split_params_and_merge_opts_array(args)
        opts         = current_midi_defaults.merge(opts)
        on_val       = opts.fetch(:on, 1)
        ports        = __resolve_midi_ports(opts)
        port         = pp_el_or_list(ports)

        if truthy?(on_val)
          ports.each do |p|
            __midi_send_timed("/#{p}/clock")
          end
          __midi_message "midi_clock_tick port: #{port}"
        else
          __midi_rest_message "midi_clock_tick port: #{port}"
        end
        nil
      end
      doc name:           :midi_clock_tick,
          introduced:     Version.new(3,0,0),
          summary:        "Send an individual MIDI clock tick",
          args:           [[]],
          returns:        :nil,
          opts: {
                          port: "MIDI port to send to",
                          on: "If specified and false/nil/0 will stop the midi clock tick message from being sent out. (Ensures all opts are evaluated in this call to `midi_clock_tick` regardless of value)."},
          accepts_block:  false,
          doc:            "Sends a MIDI clock tick message to *all* connected devices on *all* channels. Use the `port:` and `channel:` opts to restrict which MIDI ports and channels are used.

Typical MIDI devices expect the clock to send 24 ticks per quarter note (typically a beat). See `midi_clock_beat` for a simple way of sending all the ticks for a given beat.

[MIDI 1.0 Specification - System Real-Time Messages - Timing Clock](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)
",
          examples:       [
        "midi_clock_tick #=> Send an individual clock tick to all connected MIDI devices on all ports."
      ]




      def midi_start(*args)
        params, opts = split_params_and_merge_opts_array(args)
        opts         = current_midi_defaults.merge(opts)
        on_val       = opts.fetch(:on, 1)
        ports        = __resolve_midi_ports(opts)
        port         = pp_el_or_list(ports)

        if truthy?(on_val)
          ports.each do |p|
            __midi_send_timed("/#{p}/start")
          end
          __midi_message "midi_start port: #{port}"
        else
          __midi_rest_message "midi_start port: #{port}, on: 0"
        end
        nil
      end
      doc name:           :midi_start,
          introduced:     Version.new(3,0,0),
          summary:        "Send MIDI system message - start",
          args:           [[]],
          returns:        :nil,
          opts:           nil,
          accepts_block:  false,
          doc:            "Sends the MIDI start system message to *all* connected MIDI devices on *all* ports.  Use the `port:` opt to restrict which MIDI ports are used.

Start the current sequence playing. (This message should be followed with calls to `midi_clock_tick` or `midi_clock_beat`).

[MIDI 1.0 Specification - System Real-Time Messages - Start](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)
",
          examples:       [
        "midi_start #=> Send start message to all connected MIDI devices"
      ]




      def midi_stop(*args)
        params, opts = split_params_and_merge_opts_array(args)
        opts         = current_midi_defaults.merge(opts)
        on_val       = opts.fetch(:on, 1)
        ports        = __resolve_midi_ports(opts)
        port         = pp_el_or_list(ports)

        if truthy?(on_val)
          ports.each do |p|
            __midi_send_timed("/#{p}/stop")
          end
          __midi_message "midi_stop port: #{port}"
        else
          __midi_rest_message "midi_stop port: #{port}, on: 0"
        end
        nil
      end
      doc name:           :midi_stop,
          introduced:     Version.new(3,0,0),
          summary:        "Send MIDI system message - stop",
          args:           [[]],
          returns:        :nil,
          opts:           {port: "MIDI Port(s) to send the stop message to"},
          accepts_block:  false,
          doc:            "Sends the MIDI stop system message to *all* connected MIDI devices on *all* ports.  Use the `port:` opt to restrict which MIDI ports are used.

Stops the current sequence.

[MIDI 1.0 Specification - System Real-Time Messages - Start](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)
",
          examples:       [
        "midi_stop #=> Send stop message to all connected MIDI devices"
      ]




      def midi_continue(*args)
        params, opts = split_params_and_merge_opts_array(args)
        opts         = current_midi_defaults.merge(opts)
        on_val       = opts.fetch(:on, 1)
        ports        = __resolve_midi_ports(opts)
        port         = pp_el_or_list(ports)

        if truthy?(on_val)
          ports.each do |p|
            __midi_send_timed("/#{p}/continue")
          end
          __midi_message "midi_continue port: #{port}"
        else
          __midi_rest_message "midi_continue port: #{port}, on: 0"
        end
        nil
      end
      doc name:           :midi_continue,
          introduced:     Version.new(3,0,0),
          summary:        "Send MIDI system message - continue",
          args:           [[]],
          returns:        :nil,
          opts:           {port: "MIDI Port(s) to send the continue message to"},
          accepts_block:  false,
          doc:            "Sends the MIDI continue system message to *all* connected MIDI devices on *all* ports.  Use the `port:` opt to restrict which MIDI ports are used.

Upon receiving the MIDI continue event, the MIDI device(s) will continue at the point the sequence was stopped.

[MIDI 1.0 Specification - System Real-Time Messages - Continue](https://www.midi.org/specifications/item/table-1-summary-of-midi-message)
",
          examples:       [
        "midi_continue #=> Send continue message to all connected MIDI devices"
      ]



      def midi_clock_beat(*args)
        params, opts = split_params_and_merge_opts_array(args)
        opts         = current_midi_defaults.merge(opts)
        on_val       = opts.fetch(:on, 1)
        dur   = opts[:duration] || params[0] || 1
        ports = __resolve_midi_ports(opts)
        port  = pp_el_or_list(ports)

        if truthy?(on_val)

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

          ports.each do |p|
            time_warp times do |i, el|
              __midi_send_timed("/#{p}/clock")
            end
          end

          __midi_message "midi_clock_beat port: #{port}"
        else
          __midi_rest_message "midi_clock_beat port: #{port}"
        end
      end
      doc name:           :midi_clock_beat,
          introduced:     Version.new(3,0,0),
          summary:        "Send a quarter-note's worth of MIDI clock ticks",
          args:           [[:duration, :beats]],
          returns:        :nil,
         opts:           {
                          port: "MIDI port to send to",
                          on: "If specified and false/nil/0 will stop the midi clock tick messages from being sent out. (Ensures all opts are evaluated in this call to `midi_clock_beat` regardless of value)."},
          accepts_block:  false,
          doc:            "Sends enough MIDI clock ticks for one beat to *all* connected MIDI devices. Use the `port:` opt to restrict which MIDI ports are used.

The MIDI specification requires 24 clock tick events to be sent per beat. These can either be sent manually using `midi_clock_tick` or all 24 can be scheduled in one go using this fn. `midi_clock_beat` will therefore schedule for 24 clock ticks to be sent linearly spread over duration beats. This fn will automatically take into account the current BPM and any `time_warp`s.
",
          examples:       [
        "midi_clock_beat #=> Send 24 clock ticks over a period of 1 beat",
        "midi_clock_beat 0.5 #=> Send 24 clock ticks over a period of 0.5 beats",
        "
live_loop :clock do  # Create a live loop which continually sends out MIDI clock
  midi_clock_beat    # events at the current BPM
  sleep 1
end",

        "# Ensuring Clock Phase is Correct
live_loop :clock do
  midi_start if tick == 0 # Send a midi_start event the first time round the live loop only
  midi_clock_beat         # this will not just send a steady clock beat, but also ensure
  sleep 1                 # the clock phase of the MIDI device matches Sonic Pi.
end"
      ]




      def midi(*args)
        params, opts = split_params_and_merge_opts_array(args)
        opts         = current_midi_defaults.merge(opts)
        n, vel = *params

        if rest? n
          __midi_rest_message "midi :rest"
          return nil
        end

        n = normalise_transpose_and_tune_note_from_args(n, opts)

        on_val   = opts.fetch(:on, 1)
        channels = __resolve_midi_channels(opts)
        ports    = __resolve_midi_ports(opts)
        vel      = __resolve_midi_velocity(vel, opts)
        sus      = opts.fetch(:sustain, 1).to_f
        rel_vel  = opts.fetch(:release_velocity, 127)
        n        = n.round.min(0).max(127)
        chan     = pp_el_or_list(channels)
        port     = pp_el_or_list(ports)


        if truthy?(on_val)
          return midi_all_notes_off(opts) if n == :off
          ports.each do |p|
            channels.each do |c|
              __midi_send_timed_pc("/note_on", p, c, n, vel)
              time_warp sus - 0.01 do
                __midi_send_timed_pc("/note_off", p, c, n, rel_vel)
              end
            end
          end
          __midi_message "midi #{@@number_cache[n]}, #{@@number_cache[vel]}, sustain: #{sus}, port: #{port}, channel: #{chan}"
        else
          __midi_rest_message "midi #{@@number_cache[n]}, #{@@number_cache[vel]}, sustain: #{sus}, port: #{port}, channel: #{chan}, on: 0"
        end
        nil
      end
      doc name:           :midi,
          introduced:     Version.new(3,0,0),
          summary:        "Trigger and release an external synth via MIDI",
          args:           [[:note, :number], ],
          returns:        :nil,
          opts:           {sustain: "Duration of note event in beats",
                           vel:  "Velocity of note as a MIDI number",
                           on: "If specified and false/nil/0 will stop the midi on/off messages from being sent out. (Ensures all opts are evaluated in this call to `midi` regardless of value)."},
          accepts_block:  false,
          doc:            "Sends a MIDI note on event to *all* connected MIDI devices and *all* channels and then after sustain beats sends a MIDI note off event. Ensures MIDI trigger is synchronised with standard calls to play and sample. Co-operates completely with Sonic Pi's timing system including `time_warp`.

If `note` is specified as `:off` then all notes will be turned off (same as `midi_all_notes_off`).
",
          examples:       [
        "midi :e1, sustain: 0.3, vel_f: 0.5, channel: 3 # Play E, octave 1 for 0.3 beats at half velocity on channel 3 on all connected MIDI ports.",
        "midi :off, channel: 3 #=> Turn off all notes on channel 3 on all connected MIDI ports",
        "midi :e1, channel: 3, port: \"foo\" #=> Play note :E1 for 1 beats on channel 3 on MIDI port named \"foo\" only",
        "
live_loop :arp do
  midi (octs :e1, 3).tick, sustain: 0.1 # repeatedly play a ring of octaves
  sleep 0.125
end"
]


      def __resolve_midi_channels(opts)
        channels = (opts[:channel] || opts[:chan] || current_midi_channels)
        if channels == '*'
          return ['*']
        elsif is_list_like?(channels)
          return channels.map do |c|
            return ['*'] if c == '*'
            c.to_i.min(1).max(16)
          end.uniq  #requires uniq not uniq! here uniq! will return nil for [1,2,3]
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

      def __resolve_midi_note(n, opts={})
        if n = n || opts[:note]
          return note(n).round.min(0).max(127)
        else
          return 60
        end
      end

      def __resolve_midi_velocity(vel, opts={})
        if v = opts[:velocity] || opts[:vel] || vel
          return note(v).round.min(0).max(127)
        elsif v = opts[:velocity_f] || opts[:vel_f]
          return (v.to_f * 127).round.min(0).max(127)
        else
          return 127
        end
      end

      def __resolve_midi_val(val, opts={})
        if v = opts[:value] || opts[:val] || val
          return note(v).round.min(0).max(127)
        elsif v = opts[:value_f] || opts[:val_f]
          return (v.to_f * 127).round.min(0).max(127)
        else
          return 127
        end
      end

      def __resolve_midi_deltas(delta, opts={})
        if d = opts[:delta_midi]
          delta_midi = d.round.min(0).max(16383)
          delta = delta_midi / 16383.0
        elsif d = opts[:delta] || opts[:val_f] || delta
          delta = d.to_f.min(0).max(1)
          delta_midi = (d * 16383).round
        else
          delta = 0.5
          delta_midi = 8192
        end

        return delta, delta_midi
      end

      @@midi_path_cache = Hash.new {|h, k| h[k] = Hash.new() }

      def __midi_send_timed_pc(path, p, c, args0, args1=nil)
        c = -1 if c == '*'
        path = @@midi_path_cache[p][path] || @@midi_path_cache[p][path] = "/#{p}#{path}"
        if args1.nil?
          __midi_send_timed_param_2 path, c, args0
        else
          __midi_send_timed_param_3 path, c, args0, args1
        end
      end

      def __midi_send_timed(path)
        b = OSC::Blob.new(@osc_client.encoder.encode_single_message(path))
        __osc_send_api("/midi_at", b)
      end

      def __midi_send_timed_param_2(path, a, b)
        b = OSC::Blob.new(@osc_client.encoder.encode_single_message(path, [a, b]))
        __osc_send_api("/midi_at", b)
      end

      def __midi_send_timed_param_3(path, a, b, c)
        b = OSC::Blob.new(@osc_client.encoder.encode_single_message(path, [a, b, c]))
        __osc_send_api("/midi_at", b)
      end

      def __midi_send_timed_param_n(path, *args)
        b = OSC::Blob.new(@osc_client.encoder.encode_single_message(path, args))
        __osc_send_api("/midi_at", b)
      end

      def __midi_message(m)
        __delayed_message m unless __thread_locals.get(:sonic_pi_suppress_midi_logging)
      end

      def __midi_rest_message(m)
        __delayed_message m unless __thread_locals.get(:sonic_pi_suppress_midi_logging)
      end
    end
  end
end
