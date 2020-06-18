#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++
require_relative "../version"
require_relative "../util"

module SonicPi
  module Synths
    class BaseInfo
      include Util
      attr_reader :scsynth_name, :info

      def initialize
        @cached_arg_info = nil
        @scsynth_name = "#{prefix}#{synth_name}"
        merged_info = default_arg_info.merge(specific_arg_info)
        @arg_names = arg_defaults.keys

        # useful sanity check
        # @arg_names.each do |k|
        #   raise "no arg info for synth: #{@scsynth_name}, arg: #{k.inspect}" unless k.to_s.include?("_slide_") || k.to_s == "input" || merged_info[k]
        # end

        merged_info.each do |k, v|
          merged_info.delete(k) unless @arg_names.member?(k)
        end

        @info = merged_info
        # force arg_info to be cached
        arg_info
      end

      def munge_opts(studio, args_h)
        # This is an opportunity to modify the args_h prior to synth trigger
        # Result of this method will be the new args_h
        args_h
      end

      def on_start(studio, args_h)
        # do nothing

        # This will be called immediately prior to the synth
        # launching.  args_h will have been mostly normalised at this
        # stage.  (prior to a conversion to an array and all values
        # being converted to floats via .to_f)
      end

      def on_finish(studio, args_h)
        # do nothing

        # This will be called immediately after the synth has completed.
        # Execution will happen in its own independent thread.
      end

      def rrand(min, max)
        range = (min - max).abs
        r = rand(range.to_f)
        smallest = [min, max].min
        r + smallest
      end

      def alias_opts!(alias_opt, orig_opt, args_h)
        if args_h.has_key?(alias_opt) && !args_h.has_key?(orig_opt)
          args_h[orig_opt] = args_h[alias_opt]
          args_h.delete(alias_opt)
        end
        args_h
      end



      def doc
        "Please write documentation!"
      end

      def arg_defaults
        raise "please implement arg_defaults for #{self.class}"
      end

      def name
        raise "please implement name for synth info: #{self.class}"
      end

      def category
        raise "please implement category for synth info: #{self.class}"
      end

      def prefix
        ""
      end

      def synth_name
        raise "Please implement synth_name for #{self.class}"
      end

      def introduced
        raise "please implement introduced version for synth info: #{self.class}"
      end

      def trigger_with_logical_clock?
        raise "please implement trigger_with_logical_clock? for synth info: #{self.class}"
      end

      def args
        args_defaults.keys
      end

      def arg_doc(arg_name)
        info = arg_info[arg_name.to_sym]
        info[:doc] if info
      end

      def arg_default(arg_name)
        arg_defaults[arg_name.to_sym]
      end

      def ctl_validate!(*args)
        args_h = resolve_synth_opts_hash_or_array(args)

        args_h.each do |k, v|
          k_sym = k.to_sym
          arg_information = @info[k_sym]
          next unless arg_information
          arg_validations = arg_information[:validations] || []
          arg_validations.each do |v_fn, msg|
            raise "Value of opt #{k_sym.inspect} #{msg}, got #{v.inspect}." unless v_fn.call(args_h)
          end

          raise "Invalid arg modulation attempt for #{synth_name.to_sym.inspect}. Opt #{k_sym.inspect} is not modulatable" unless arg_information[:modulatable]

        end
      end

      def validate!(*args)
        args_h = resolve_synth_opts_hash_or_array(args)

        args_h.each do |k, v|
          k_sym = k.to_sym
          #        raise "Value of argument #{k_sym.inspect} must be a number, got #{v.inspect}." unless v.is_a? Numeric

          arg_validations(k_sym).each do |v_fn, msg|
            raise "Value of opt #{k_sym.inspect} #{msg}, got #{v.inspect}." unless v_fn.call(args_h)
          end
        end
      end

      def arg_validations(arg_name)
        arg_information = @info[arg_name] || {}
        arg_information[:validations] || []
      end

      def bpm_scale_args
        return @cached_bpm_scale_args if @cached_bpm_scale_args

        args_to_scale = []
        @info.each do |k, v|
          args_to_scale << k if v[:bpm_scale]
        end

        @cached_bpm_scale_args = args_to_scale
      end

      def buffer_args
        return @cached_buffer_args if @cached_buffer_args

        buffer_args = []
        @info.each do |k, v|
          buffer_args << k if v[:buffer]
        end
        @cached_buffer_args = buffer_args
      end

      def midi_args
        return @cached_midi_args if @cached_midi_args

        midi_args = []
        @info.each do |k, v|
          midi_args << k if v[:midi]
        end
        @cached_midi_args = midi_args
      end

      def slide_args
        return @cached_slide_args if @cached_slide_args

        slide_args = []
        @info.each do |k, v|
          slide_args << k if v[:bpm_scale] && k.to_s.end_with?("slide")
        end

        @cached_slide_args = slide_args
      end

      def slide_arg_defaults
        return @cached_slide_arg_defaults if @cached_slide_arg_defaults
        slide_arg_defaults = slide_args.reduce({}) do |res, a|
          res[a] = arg_defaults[a]
        end

        @cached_slide_arg_defaults = slide_arg_defaults
      end

      def arg_info
        return @cached_arg_info if @cached_arg_info

        res = {}
        arg_defaults.each do |arg, default|
          if m = /(.*)_slide/.match(arg.to_s) then
            # modify stem opt (creating info if it doesn't exist)
            parent = m[1].to_sym
            new_info = res[parent] || {}
            new_info[:slidable] = true
            res[parent] = new_info
          else
            default_info = @info[arg] || {}
            constraints = (default_info[:validations] || []).map{|el| el[1]}
            new_info = {}
            new_info[:doc] = default_info[:doc]
            new_info[:default] = default_info[:default] || default
            new_info[:bpm_scale] = default_info[:bpm_scale]
            new_info[:constraints] = constraints
            new_info[:modulatable] = default_info[:modulatable]
            res[arg] = new_info
          end
        end

        @cached_arg_info = res

      end

      def kill_delay(args_h)
        1
      end

      def generic_slide_doc(k)
        return "Amount of time (in beats) for the #{k} value to change. A long #{k}_slide value means that the #{k} takes a long time to slide from the previous value to the new value. A #{k}_slide of 0 means that the #{k} instantly changes to the new value."
      end

      def generic_slide_curve_doc(k)
        return "Shape of the slide curve (only honoured if slide shape is 5). 0 means linear and positive and negative numbers curve the segment up and down respectively."
      end

      def generic_slide_shape_doc(k)
        return "Shape of curve. 0: step, 1: linear, 3: sine, 4: welch, 5: custom (use *_slide_curve: opt e.g. amp_slide_curve:), 6: squared, 7: cubed. "
      end

      private

      def v_buffer_like(arg)
        l = lambda do |args|
          a = args[arg]
          a &&
            (
            # straight up buffer
            a.is_a?(Buffer)  ||

            # string or symbol representing the
            # buffers name
            a.is_a?(String)  ||
            a.is_a?(Symbol)  ||

            # vector description of buffer consisting of two
            # arguments, name and size:
            # [:foo, 3]
            # (A buffer called foo of duration 3)
            (
              is_list_like?(a) &&
              a.size == 2 &&
              (a[0].is_a?(Symbol) || a[0].is_a?(String)) &&
              a[1].is_a?(Numeric)
              ))
        end
        [l, "must be a buffer description. Such as a buffer, :foo, \"foo\", or [:foo, 4]"]
      end

      def v_sum_less_than_oet(arg1, arg2, max)
        [lambda{|args| (args[arg1] + args[arg2]) <= max}, "added to #{arg2.to_sym} must be less than or equal to #{max}"]
      end

      def v_positive(arg)
        [lambda{|args| args[arg] >= 0}, "must be zero or greater"]
      end

      def v_positive_not_zero(arg)
        [lambda{|args| args[arg] > 0}, "must be greater than zero"]
      end

      def v_between_inclusive(arg, min, max)
        [lambda{|args| args[arg] >= min && args[arg] <= max}, "must be a value between #{min} and #{max} inclusively"]
      end

      def v_between_exclusive(arg, min, max)
        [lambda{|args| args[arg] > min && args[arg] < max}, "must be a value between #{min} and #{max} exclusively"]
      end

      def v_less_than(arg,  max)
        [lambda{|args| args[arg] < max}, "must be a value less than #{max}"]
      end

      def v_less_than_oet(arg,  max)
        [lambda{|args| args[arg] <= max}, "must be a value less than or equal to #{max}"]
      end

      def v_greater_than(arg,  min)
        [lambda{|args| args[arg] > min}, "must be a value greater than #{min}"]
      end

      def v_greater_than_oet(arg,  min)
        [lambda{|args| args[arg] >= min}, "must be a value greater than or equal to #{min}"]
      end

      def v_one_of(arg, valid_options)
        [lambda{|args| valid_options.include?(args[arg])}, "must be one of the following values: #{valid_options.inspect}"]
      end

      def v_not_zero(arg)
        [lambda{|args| args[arg] != 0}, "must not be zero"]
      end

      def default_arg_info
        {
          :note =>
          {
            :doc => "Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`",
            :validations => [v_positive(:note)],
            :modulatable => true
          },

          :note_slide =>
          {
            :doc => "Amount of time (in beats) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.",
            :validations => [v_positive(:note_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :note_slide_shape =>
          {
            :doc => "Shape of curve for note slide  0: step, 1: linear, 2: exponential, 3: sine, 4: welch, 5: custom (use *_curve opt), 6: squared, 7: cubed, 8: hold",
            :validations => [v_one_of(:note_slide_shape, [0, 1, 2, 3, 4, 6, 7, 8])],
            :modulatable => false
          },

          :note_slide_curve =>
          {
            :doc => "Control curvature for note slide. Only used if *_slide_shape is 5. 0 means linear, positive and negative numbers curve the segment up and down.",
            :modulatable => false
          },

          :amp =>
          {
            :doc => "The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, they will just reduce the quality of all the sounds currently being played (due to compression.)",
            :validations => [v_positive(:amp)],
            :modulatable => true
          },

          :amp_slide =>
          {
            :doc => "Amount of time (in beats) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.",
            :validations => [v_positive(:amp_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :pan =>
          {

            :doc => "Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the sound is completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.",
            :validations => [v_between_inclusive(:pan, -1, 1)],
            :modulatable => true
          },

          :pan_slide =>
          {
            :doc => "Amount of time (in beats) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position.",
            :validations => [v_positive(:pan_slide)],
            :modulatable => true,
            :bpm_scale => true
          },


          :attack =>
          {
            :doc => "Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + decay + sustain + release.",
            :validations => [v_positive(:attack)],
            :modulatable => false,
            :bpm_scale => true
          },

          :decay =>
          {
            :doc => "Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).",
            :validations => [v_positive(:decay)],
            :modulatable => false,
            :bpm_scale => true
          },

          :sustain =>
          {
            :doc => "Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.",
            :validations => [v_positive(:sustain)],
            :modulatable => false,
            :bpm_scale => true
          },

          :release =>
          {
            :doc => "Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + decay + sustain + release.",
            :validations => [v_positive(:release)],
            :modulatable => false,
            :bpm_scale => true
          },

          :attack_level =>
          {
            :doc => "Amplitude level reached after attack phase and immediately before decay phase",
            :validations => [v_positive(:attack_level)],
            :modulatable => false
          },

          :decay_level =>
          {
            :doc => "Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set",
            :validations => [v_positive(:decay_level)],
            :modulatable => false
          },

          :sustain_level =>
          {
            :doc => "Amplitude level reached after decay phase and immediately before release phase.",
            :validations => [v_positive(:sustain_level)],
            :modulatable => false
          },

          :env_curve =>
          {
            :doc => "Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed",
            :validations => [v_one_of(:env_curve, [1, 2, 3, 4, 6, 7])],
            :modulatable => false
          },

          :cutoff =>
          {
            :doc => "MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.",
            :validations => [v_positive(:cutoff), v_less_than(:cutoff, 131)],
            :modulatable => true,
            :midi => true
          },

          :cutoff_slide =>
          {
            :doc => "Amount of time (in beats) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value.",
            :validations => [v_positive(:cutoff_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :detune =>
          {
            :doc => "Distance (in MIDI notes) between components of sound. Affects thickness, sense of tuning and harmony. Tiny values such as 0.1 create a thick sound. Larger values such as 0.5 make the tuning sound strange. Even bigger values such as 5 create chord-like sounds.",
            :validations => [],
            :modulatable => true
          },

          :detune_slide =>
          {
            :doc => generic_slide_doc(:detune),
            :validations => [v_positive(:detune_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :mod_phase =>
          {
            :doc => "Phase duration in beats of oscillations between the two notes. Time it takes to switch between the notes.",
            :validations => [v_positive_not_zero(:mod_phase)],
            :modulatable => true,
            :bpm_scale => true
          },

          :mod_phase_offset =>
          {
            :doc => "Initial modulation phase offset (a value between 0 and 1).",
            :validations => [v_between_inclusive(:mod_phase_offset, 0, 1)],
            :modulatable => false
          },

          :mod_phase_slide =>
          {
            :doc => generic_slide_doc(:mod_phase),
            :validations => [v_positive(:mod_phase_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :mod_range =>
          {
            :doc => "The size of gap between modulation notes. A gap of 12 is one octave.",
            :modulatable => true
          },

          :mod_range_slide =>
          {
            :doc => generic_slide_doc(:mod_range),
            :validations => [v_positive(:mod_range_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :res =>
          {
            :doc => "Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.",
            :validations => [v_positive(:res), v_less_than(:res, 1)],
            :modulatable => true
          },

          :res_slide =>
          {
            :doc => generic_slide_doc(:res),
            :validations => [v_positive(:res_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :pulse_width =>
          {
            :doc => "The width of the pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Different values will change the timbre of the sound. Only valid if wave is type pulse.",
            :validations => [v_between_exclusive(:pulse_width, 0, 1)],
            :modulatable => true
          },

          :pulse_width_slide =>
          {
            :doc => "Time in beats for pulse width to change.",
            :validations => [v_positive(:pulse_width_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :mod_pulse_width =>
          {
            :doc => "The width of the modulated pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Only valid if mod wave is type pulse.",
            :validations => [v_between_exclusive(:mod_pulse_width, 0, 1)],
            :modulatable => true
          },

          :mod_pulse_width_slide =>
          {
            :doc => "Time in beats for modulated pulse width to change.",
            :validations => [v_positive(:mod_pulse_width_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :mod_wave =>
          {
            :doc => "Wave shape of mod wave. 0=saw wave, 1=pulse, 2=triangle wave and 3=sine wave.",
            :validations => [v_one_of(:mod_wave, [0, 1, 2, 3])],
            :modulatable => true
          },

          :mod_invert_wave =>
          {
            :doc => "Invert mod waveform (i.e. flip it on the y axis). 0=normal wave, 1=inverted wave.",
            :validations => [v_one_of(:mod_invert_wave, [0, 1])],
            :modulatable => true
          }

        }
      end

      def specific_arg_info
        {}
      end

    end



    class SynthInfo < BaseInfo
      def category
        :general
      end

      def prefix
        "sonic-pi-"
      end
    end

    class SonicPiSynth < SynthInfo
      def user_facing?
        true
      end
    end

    class SoundIn < SonicPiSynth
      def name
        "Sound In"
      end

      def introduced
        Version.new(2,10,0)
      end

      def synth_name
        "sound_in"
      end

      def doc
        "Treat sound card input as a synth. If your audio card has inputs, you may use this synth to feed the incoming audio into Sonic Pi. This synth will read in a single mono audio stream - for example from a standard microphone or guitar. See `:sound_in_stereo` for a similar synth capable of reading in a stereo signal.

As with all Sonic Pi synths, there is a default envelope which determines the duration of the lifetime of the synth. Therefore, to get a continuous stream of audio, you need to place consecutive calls to this synth in iteration or a `live_loop`. For example:

```
live_loop :playback do
```

```
   synth :sound_in, sustain: 8
```

```
   sleep 8
```

```
end
```

Note that if the microphone and speaker are close together (on a laptop or in a small room) you will potentially get a harsh feedback sound.

Also, note that audio in isn't yet supported on Raspberry Pi."
      end

      def arg_defaults
        {
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 1,
          :release => 0,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 0,

          :input => 1
        }
      end

      def specific_arg_info
        {
          :input =>
          {
            :doc => "Sound card input channel to obtain audio from. Indexing starts at 1 so input 1 represents the first channel, and channel 2 can be represented by `input: 2`",
            :validations => [v_greater_than_oet(:input, 1)],
            :modulatable => true,
          }
        }
      end

    end

    class SoundInStereo < SoundIn
      def name
        "Sound In Stereo"
      end

      def synth_name
        "sound_in_stereo"
      end

      def specific_arg_info
        {
          :input =>
          {
            :doc => "First of two consecutive sound card input channels to obtain audio from. Indexing starts at 1 so input 1 represents the first channel, and channel 2 can be represented by `input: 2`",
            :validations => [v_greater_than_oet(:input, 1)],
            :modulatable => true,
          }
        }
      end

      def doc
        "Treat sound card input as a synth. If your audio card has inputs, you may use this synth to feed the incoming audio into Sonic Pi. This synth will read in a stereo audio stream - for example from a stereo microphone or external stereo keyboard. See `:sound_in` for a similar synth capable of reading in a mono signal. The stereo input is expected to be on consecutive sound card channels.

As with all Sonic Pi synths, there is a default envelope which determines the duration of the lifetime of the synth. Therefore, to get a continuous stream of audio, you need to place consecutive calls to this synth in iteration or a `live_loop`. For example:

```
live_loop :playback do
```

```
   synth :sound_in_stereo, sustain: 8
```

```
   sleep 8
```

```
end
```

Note that if the microphone and speaker are close together (on a laptop or in a small room) you will potentially get a harsh feedback sound.

Also, note that audio in isn't yet supported on Raspberry Pi."
      end
    end



    class DullBell < SonicPiSynth
      def name
        "Dull Bell"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "dull_bell"
      end

      def doc
        "A simple dull discordant bell sound."
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2
        }
      end
    end

    class PrettyBell < DullBell
      def name
        "Pretty Bell"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "pretty_bell"
      end

      def doc
        "A pretty bell sound. Works well with short attacks and long decays."
      end
    end

    class Beep < SonicPiSynth
      def name
        "Sine Wave"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "beep"
      end

      def doc
        "A simple pure sine wave. The sine wave is the simplest, purest sound there is and is the fundamental building block of all noise. The mathematician Fourier demonstrated that any sound could be built out of a number of sine waves (the more complex the sound, the more sine waves needed). Have a play combining a number of sine waves to design your own sounds!"
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2
        }
      end
    end

    class Saw < Beep
      def name
        "Saw Wave"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "saw"
      end

      def doc
        "A saw wave with a low pass filter. Great for using with FX such as the built in low pass filter (available via the cutoff arg) due to the complexity and thickness of the sound."
      end

      def arg_defaults
        super.merge({
          cutoff: 100,
          cutoff_slide: 0,
          cutoff_slide_shape: 1,
          cutoff_slide_curve: 0
        })
      end
    end


    class Square < SonicPiSynth
      def name
        "Square Wave"
      end

      def introduced
        Version.new(2,2,0)
      end

      def synth_name
        "square"
      end

      def doc
        "A simple square wave with a low pass filter. The square wave is thick and heavy with lower notes and is a great ingredient for bass sounds. If you wish to modulate the width of the square wave see the synth pulse."
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2,

          :cutoff => 100,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0
        }
      end
    end

    class Pulse < Square
      def name
        "Pulse Wave"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "pulse"
      end

      def doc
        "A simple pulse wave with a low pass filter. This defaults to a square wave, but the timbre can be changed dramatically by adjusting the pulse_width arg between 0 and 1. The pulse wave is thick and heavy with lower notes and is a great ingredient for bass sounds."
      end

      def arg_defaults
        super.merge({
                      :pulse_width => 0.5,
                      :pulse_width_slide => 0,
                      :pulse_width_slide_shape => 1,
                      :pulse_width_slide_curve => 0})
      end
    end

    class SubPulse < Pulse
      def name
        "Pulse Wave with sub"
      end

      def introduced
        Version.new(2,6,0)
      end

      def synth_name
        "subpulse"
      end

      def doc
        "A pulse wave with a sub sine wave passed through a low pass filter. The pulse wave is thick and heavy with lower notes and is a great ingredient for bass sounds - especially with the sub wave."
      end

      def arg_defaults
        super.merge({
                      :sub_amp => 1,
                      :sub_amp_slide => 0,
                      :sub_amp_slide_shape => 1,
                      :sub_amp_slide_curve => 0,
                      :sub_detune => -12,
                      :sub_detune_slide => 0,
                      :sub_detune_slide_shape => 1,
                      :sub_detune_slide_curve => 0})
      end

      def specific_arg_info
        {
          :sub_amp =>
          {
            :doc => "Amplitude for the additional sine wave.",
            :validations => [],
            :modulatable => true
          },

          :sub_amp_slide =>
          {
            :doc => generic_slide_doc(:sub_amp),
            :validations => [v_positive(:sub_amp_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :sub_detune =>
          {
            :doc => "Amount of detune from the note for the additional sine wave. Default is -12",
            :validations => [],
            :modulatable => true
          },

          :sub_detune_slide =>
          {
            :doc => generic_slide_doc(:sub_detune),
            :validations => [v_positive(:sub_detune_slide)],
            :modulatable => true,
            :bpm_scale => true
          },
        }
      end
    end

    class Tri < Pulse
      def name
        "Triangle Wave"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "tri"
      end

      def doc
        "A simple triangle wave with a low pass filter."
      end
    end

    class DSaw < SonicPiSynth
      def name
        "Detuned Saw wave"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "dsaw"
      end

      def doc
        "A pair of detuned saw waves passed through a low pass filter. Two saw waves with slightly different frequencies generates a nice thick sound which is the basis for a lot of famous synth sounds. Thicken the sound by increasing the detune value, or create an octave-playing synth by choosing a detune of 12 (12 MIDI notes is an octave)."
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2,

          :cutoff => 100,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,
          :detune => 0.1,
          :detune_slide => 0,
          :detune_slide_shape => 1,
          :detune_slide_curve => 0,
        }
      end
    end

    class DTri < DSaw
      def name
        "Detuned Triangle Wave"
      end

      def introduced
        Version.new(2, 10, 0)
      end

      def synth_name
        "dtri"
      end

      def doc
        "A pair of detuned triangle waves passed through a low pass filter. Two pulse waves with slightly different frequencies generates a nice thick sound which can be used as a basis for some nice bass sounds. Thicken the sound by increasing the detune value, or create an octave-playing synth by choosing a detune of 12 (12 MIDI notes is an octave)."
      end
    end

    class DPulse < DSaw
      def name
        "Detuned Pulse Wave"
      end

      def introduced
        Version.new(2, 8, 0)
      end

      def synth_name
        "dpulse"
      end

      def doc
        "A pair of detuned pulse waves passed through a low pass filter. Two pulse waves with slightly different frequencies generates a nice thick sound which can be used as a basis for some nice bass sounds. Thicken the sound by increasing the detune value, or create an octave-playing synth by choosing a detune of 12 (12 MIDI notes is an octave). Each pulse wave can also have individual widths (although the default is for the detuned pulse to mirror the width of the main pulse)."
      end

      def arg_defaults
        super.merge({
                      :pulse_width => 0.5,
                      :pulse_width_slide => 0,
                      :pulse_width_slide_shape => 1,
                      :pulse_width_slide_curve => 0,
                      :dpulse_width => :pulse_width,
                      :dpulse_width_slide => :pulse_width_slide,
                      :dpulse_width_slide_shape => :pulse_width_slide_shape,
                      :dpulse_width_slide_curve => :pulse_width_slide_curve})
      end

      def specific_arg_info
        {
          :pulse_width =>
          {
            :doc => "The width of the pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Different values will change the timbre of the sound. Only valid if wave is type pulse.",
            :validations => [v_between_exclusive(:pulse_width, 0, 1)],
            :modulatable => true
          },

          :pulse_width_slide =>
          {
            :doc => "Time in beats for pulse width to change.",
            :validations => [v_positive(:pulse_width_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :dpulse_width =>
          {
            :doc => "The width of the second detuned pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Different values will change the timbre of the sound. Only valid if wave is type pulse.",
            :validations => [v_between_exclusive(:dpulse_width, 0, 1)],
            :modulatable => true
          },

          :dpulse_width_slide =>
          {
            :doc => "Time in beats for second detuned pulse width to change.",
            :validations => [v_positive(:dpulse_width_slide)],
            :modulatable => true,
            :bpm_scale => true
          }
        }

      end
    end


    class FM < SonicPiSynth
      def name
        "Basic FM synthesis"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "fm"
      end

      def doc
        "A sine wave with a fundamental frequency which is modulated at audio rate by another sine wave with a specific modulation, division and depth. Useful for generating a wide range of sounds by playing with the divisor and depth params. Great for deep powerful bass and crazy 70s sci-fi sounds."
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2,

          :cutoff => 100,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,

          :divisor => 2,
          :divisor_slide => 0,
          :divisor_slide_shape => 1,
          :divisor_slide_curve => 0,
          :depth => 1,
          :depth_slide => 0,
          :depth_slide_shape => 1,
          :depth_slide_curve => 0,
        }
      end

      def specific_arg_info
        {
          :divisor =>
          {
            :doc => "Modifies the frequency of the modulator oscillator relative to the carrier. Don't worry too much about what this means - just try different numbers out!",
            :validations => [],
            :modulatable => true
          },

          :divisor_slide =>
          {
            :doc => generic_slide_doc(:divisor),
            :validations => [v_positive(:divisor_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :depth =>
          {
            :doc => "Modifies the depth of the carrier wave used to modify fundamental frequency. Don't worry too much about what this means - just try different numbers out!",
            :validations => [],
            :modulatable => true
          },

          :depth_slide =>
          {
            :doc => generic_slide_doc(:depth),
            :validations => [v_positive(:depth_slide)],
            :modulatable => true,
            :bpm_scale => true
          }
        }

      end
    end

    class ModFM < FM

      def name
        "Basic FM synthesis with frequency modulation."
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "mod_fm"
      end

      def doc
        "The FM synth modulating between two notes - the duration of the modulation can be modified using the mod_phase arg, the range (number of notes jumped between) by the mod_range arg and the width of the jumps by the mod_width param. The FM synth is a sine wave with a fundamental frequency which is modulated at audio rate by another sine wave with a specific modulation, division and depth. Useful for generating a wide range of sounds by playing with the `:divisor` and `:depth` params. Great for deep powerful bass and crazy 70s sci-fi sounds."
      end

      def arg_defaults
        super.merge({
                      :mod_phase => 0.25,
                      :mod_range => 5,
                      :mod_pulse_width => 0.5,
                      :mod_phase_offset => 0,
                      :mod_invert_wave => 0,
                      :mod_wave => 1
                    })
      end


    end

    class ModSaw < SonicPiSynth
      def name
        "Modulated Saw Wave"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "mod_saw"
      end

      def doc
        "A saw wave passed through a low pass filter which modulates between two separate notes via a variety of control waves."
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2,

          :cutoff => 100,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,
          :mod_phase => 0.25,
          :mod_phase_slide => 0,
          :mod_phase_slide_shape => 1,
          :mod_phase_slide_curve => 0,
          :mod_range => 5,
          :mod_range_slide => 0,
          :mod_range_slide_shape => 1,
          :mod_range_slide_curve => 0,
          :mod_pulse_width => 0.5,
          :mod_pulse_width_slide => 0,
          :mod_pulse_width_slide_shape => 1,
          :mod_pulse_width_slide_curve => 0,
          :mod_phase_offset => 0,
          :mod_invert_wave => 0,
          :mod_wave => 1

        }
      end
    end

    class ModDSaw < SonicPiSynth
      def name
        "Modulated Detuned Saw Waves"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "mod_dsaw"
      end

      def doc
        "A pair of detuned saw waves (see the dsaw synth) which are modulated between two fixed notes at a given rate."
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2,

          :cutoff => 100,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,
          :mod_phase => 0.25,

          :mod_phase_slide => 0,
          :mod_phase_slide_shape => 1,
          :mod_phase_slide_curve => 0,
          :mod_range => 5,
          :mod_range_slide => 0,
          :mod_range_slide_shape => 1,
          :mod_range_slide_curve => 0,
          :mod_pulse_width => 0.5,
          :mod_pulse_width_slide => 0,
          :mod_pulse_width_slide_shape => 1,
          :mod_pulse_width_slide_curve => 0,
          :mod_phase_offset => 0,
          :mod_invert_wave => 0,
          :mod_wave => 1,
          :detune => 0.1,
          :detune_slide => 0,
          :detune_slide_shape => 1,
          :detune_slide_curve => 0,
        }
      end
    end


    class ModSine < SonicPiSynth
      def name
        "Modulated Sine Wave"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "mod_sine"
      end

      def doc
        "A sine wave passed through a low pass filter which modulates between two separate notes via a variety of control waves."
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2,

          :cutoff => 100,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,
          :mod_phase => 0.25,
          :mod_phase_slide => 0,
          :mod_phase_slide_shape => 1,
          :mod_phase_slide_curve => 0,
          :mod_range => 5,
          :mod_range_slide => 0,
          :mod_range_slide_shape => 1,
          :mod_range_slide_curve => 0,
          :mod_pulse_width => 0.5,
          :mod_pulse_width_slide => 0,
          :mod_pulse_width_slide_shape => 1,
          :mod_pulse_width_slide_curve => 0,
          :mod_phase_offset => 0,
          :mod_invert_wave => 0,
          :mod_wave => 1

        }
      end
    end

    class ModTri < SonicPiSynth
      def name
        "Modulated Triangle Wave"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "mod_tri"
      end

      def doc
        "A triangle wave passed through a low pass filter which modulates between two separate notes via a variety of control waves."
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2,

          :cutoff => 100,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,
          :mod_phase => 0.25,
          :mod_phase_slide => 0,
          :mod_phase_slide_shape => 1,
          :mod_phase_slide_curve => 0,
          :mod_range => 5,
          :mod_range_slide => 0,
          :mod_range_slide_shape => 1,
          :mod_range_slide_curve => 0,
          :mod_pulse_width => 0.5,
          :mod_pulse_width_slide => 0,
          :mod_pulse_width_slide_shape => 1,
          :mod_pulse_width_slide_curve => 0,
          :mod_phase_offset => 0,
          :mod_invert_wave => 0,
          :mod_wave => 1
        }
      end
    end


    class ModPulse < SonicPiSynth
      def name
        "Modulated Pulse"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "mod_pulse"
      end

      def doc
        "A pulse wave with a low pass filter modulating between two notes via a variety of control waves (see mod_wave: arg). The pulse wave defaults to a square wave, but the timbre can be changed dramatically by adjusting the pulse_width arg between 0 and 1."
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2,

          :cutoff => 100,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,
          :mod_phase => 0.25,
          :mod_phase_slide => 0,
          :mod_phase_slide_shape => 1,
          :mod_phase_slide_curve => 0,
          :mod_range => 5,
          :mod_range_slide => 0,
          :mod_range_slide_shape => 1,
          :mod_range_slide_curve => 0,
          :mod_pulse_width => 0.5,
          :mod_pulse_width_slide => 0,
          :mod_pulse_width_slide_shape => 1,
          :mod_pulse_width_slide_curve => 0,
          :mod_phase_offset => 0,
          :mod_invert_wave => 0,
          :mod_wave => 1,
          :pulse_width => 0.5,
          :pulse_width_slide => 0,
          :pulse_width_slide_shape => 1,
          :pulse_width_slide_curve => 0,
        }
      end
    end


    class TB303 < SonicPiSynth
      def name
        "TB-303 Emulation"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "tb303"
      end

      def doc
        "Emulation of the classic Roland TB-303 Bass Line synthesiser. Overdrive the res (i.e. use very large values) for that classic late 80s acid sound."
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2,

          :cutoff => 120,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,
          :cutoff_min => 30,
          :cutoff_min_slide => 0,
          :cutoff_min_slide_shape => 1,
          :cutoff_min_slide_curve => 0,
          :cutoff_attack => :attack,
          :cutoff_decay => :decay,
          :cutoff_sustain => :sustain,
          :cutoff_release => :release,
          :cutoff_attack_level => 1,
          :cutoff_decay_level => :cutoff_sustain_level,
          :cutoff_sustain_level => 1,
          :res => 0.9,
          :res_slide => 0,
          :res_slide_shape => 1,
          :res_slide_curve => 0,
          :wave => 0,
          :pulse_width => 0.5,
          :pulse_width_slide => 0,
          :pulse_width_slide_shape => 1,
          :pulse_width_slide_curve => 0,
        }
      end

      def specific_arg_info
        {
          :cutoff_min =>
          {
            :doc => "The minimum cutoff value.",
            :validations => [v_less_than_oet(:cutoff_min, 130)],
            :modulatable => true,
            :midi => true
          },

          :cutoff_min_slide =>
          {
            :doc => generic_slide_doc(:cutoff_min),
            :validations => [v_positive(:cutoff_min_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :cutoff =>
          {
            :doc => "The maximum cutoff value as a MIDI note",
            :validations => [v_less_than_oet(:cutoff, 130)],
            :modulatable => true,
            :midi => true
          },

          :cutoff_slide =>
          {
            :doc => generic_slide_doc(:cutoff),
            :validations => [v_positive(:cutoff_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :cutoff_attack_level =>
          {
            :doc => "The peak cutoff (value of cutoff at peak of attack) as a value between 0 and 1 where 0 is the :cutoff_min and 1 is the :cutoff value",
            :validations => [v_between_inclusive(:cutoff_attack_level, 0, 1)],
            :modulatable => false
          },

          :cutoff_decay_level =>
          {
            :doc => "The level of cutoff after the decay phase as a value between 0 and 1 where 0 is the :cutoff_min and 1 is the :cutoff value",
            :validations => [v_between_inclusive(:cutoff_decay_level, 0, 1)],
            :modulatable => false
          },


          :cutoff_sustain_level =>
          {
            :doc => "The sustain cutoff (value of cutoff at sustain time) as a value between 0 and 1 where 0 is the :cutoff_min and 1 is the :cutoff value.",
            :validations => [v_between_inclusive(:cutoff_sustain_level, 0, 1)],
            :modulatable => false
          },

          :cutoff_attack =>
          {
            :doc => "Attack time for cutoff filter. Amount of time (in beats) for sound to reach full cutoff value. Default value is set to match amp envelope's attack value.",
            :validations => [v_positive(:cutoff_attack)],
            :modulatable => false,
            :default => "attack",
            :bpm_scale => true
          },

          :cutoff_decay =>
          {
            :doc => "Decay time for cutoff filter. Amount of time (in beats) for sound to move from full cutoff value (cutoff attack level) to the cutoff sustain level. Default value is set to match amp envelope's decay value.",
            :validations => [v_positive(:cutoff_decay)],
            :modulatable => false,
            :default => "decay",
            :bpm_scale => true
          },

          :cutoff_sustain =>
          {
            :doc => "Amount of time for cutoff value to remain at sustain level in beats. Default value is set to match amp envelope's sustain value.",
            :validations => [v_positive(:cutoff_sustain)],
            :modulatable => false,
            :default => "sustain",
            :bpm_scale => true
          },

          :cutoff_release =>
          {
            :doc => "Amount of time (in beats) for sound to move from cutoff sustain value to cutoff min value. Default value is set to match amp envelope's release value.",
            :validations => [v_positive(:cutoff_release)],
            :modulatable => false,
            :default => "release",
            :bpm_scale => true
          },

          :cutoff_env_curve =>
          {
            :doc => "Select the shape of the curve between levels in the cutoff envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed",
            :validations => [v_one_of(:cutoff_env_curve, [1, 2, 3, 4, 6, 7])],
            :modulatable => false
          },

          :wave =>
          {
            :doc => "Wave type - 0 saw, 1 pulse, 2 triangle. Different waves will produce different sounds.",
            :validations => [v_one_of(:wave, [0, 1, 2])],
            :modulatable => true
          },

        }
      end
    end

    class Supersaw < SonicPiSynth
      def name
        "Supersaw"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "supersaw"
      end

      def doc
        "Thick swirly saw waves sparkling and moving about to create a rich trancy sound."
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2,

          :cutoff => 130,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,
          :res => 0.7,
          :res_slide => 0,
          :res_slide_shape => 1,
          :res_slide_curve => 0,

        }
      end
    end

    class Hoover < SonicPiSynth
      def name
        "Hoover"
      end

      def introduced
        Version.new(2,6,0)
      end

      def synth_name
        "hoover"
      end

      def doc
        "Classic early 90's rave synth - 'a sort of slurry chorussy synth line like the classic Dominator by Human Resource'. Based on Dan Stowell's implementation in SuperCollider and Daniel Turczanski's port to Overtone. Works really well with portamento (see docs for the 'control' method)."
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,
          :attack => 0.05,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2,
          :cutoff => 130,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,
          :res => 0.1,
          :res_slide => 0,
          :res_slide_shape => 1,
          :res_slide_curve => 0,
        }
      end
    end

    class SynthViolin < SonicPiSynth
      def name
        "Blade Runner style strings"
      end

      def introduced
        Version.new(2,6,0)
      end

      def synth_name
        "blade"
      end

      def doc
        "Straight from the 70s, evoking the mists of Blade Runner, this simple electro-style string synth is based on filtered saw waves and a variable vibrato."
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,
          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2,

          :cutoff => 100,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,

          :vibrato_rate => 6,
          :vibrato_rate_slide_shape => 1,
          :vibrato_rate_slide_curve => 0,
          :vibrato_depth => 0.15,
          :vibrato_depth_slide_shape => 1,
          :vibrato_depth_slide_curve => 0,
          :vibrato_delay => 0.5,
          :vibrato_onset => 0.1,
        }
      end

      def specific_arg_info
        {
          :vibrato_rate => {
            :doc => "Number of wobbles per second. For realism this should be between 6 and 8, maybe even faster for really high notes.",
            :validations => [v_greater_than_oet(:vibrato_rate, 0.0), v_less_than_oet(:vibrato_rate, 20.0)],
            :modulatable => true
          },
          :vibrato_depth =>
          {
            :doc => "Amount of variation around the central note. 1 is the sensible maximum (but you can go up to 5 if you want a special effect), 0 would mean no vibrato. Works well around 0.15 but you can experiment.",
            :validations => [v_greater_than_oet(:vibrato_depth, 0.0), v_less_than_oet(:vibrato_depth, 5.0)],
            :modulatable => true
          },
          :vibrato_delay =>
          {
            :doc => "How long in seconds before the vibrato kicks in.",
            :validations => [v_positive(:vibrato_delay)],
            :modulatable => false
          },
          :vibrato_onset =>
          {
            :doc => "How long in seconds before the vibrato reaches full power.",
            :validations => [v_positive(:vibrato_onset)],
            :modulatable => false
          },
        }
      end
    end

    class SynthPluck < SonicPiSynth
      def name
        "SynthPluck"
      end

      def introduced
        Version.new(2,10,0)
      end

      def synth_name
        "pluck"
      end

      def doc
        "A basic plucked string synthesiser that uses Karplus-Strong synthesis. Note that due to the plucked nature of this synth the envelope opts such as `attack:`, `sustain:` and `release:` do not work as expected. They can only shorten the natural length of the note, not prolong it. Also, the `note:` opt will only honour whole tones."
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,
          :attack => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay => 0,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :noise_amp => 0.8,
          :max_delay_time => 0.125,
          :pluck_decay => 30,
          :coef => 0.3
        }
      end

      def specific_arg_info
        {
          :note =>
          {
            :doc => "Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, `:C`, `:C2`, `:Eb4`, or `:Ds3`. Note that the piano synth can only play whole tones such as 60 and does not handle floats such as 60.3",
            :validations => [v_positive(:note)],
            :modulatable => true
          },

          :noise_amp => {
            :doc => "Amplitude of source (pink) noise.",
            :validations => [v_between_inclusive(:noise_amp, 0, 1)],
            :modulatable => false},

          :max_delay_time => {
            :doc => "Maximum length of the delay line buffer.",
            :validations => [v_between_inclusive(:max_delay_time, 0.125, 1)],
            :modulatable => false},

          :pluck_decay => {
            :doc => "How long the pluck takes to stabilise on a note. This doesn't have a dramatic effect on the sound.",
            :validations => [v_between_inclusive(:pluck_decay, 1, 100)],
            :modulatable => false},

          :coef =>
          {
            :doc => "Coefficient of the internal OnePole filter. Values around zero are resonant and bright, values towards 1 sound more dampened and cutoff. It's a little bit like playing nearer the soundhole/fingerboard for values near zero and more toward the bridge for values approaching one, although this isn't an exact comparison.",
            :validations => [v_between_inclusive(:coef, -1, 1)],
            :modulatable => false
          },

          :decay =>
          {
            :doc => "Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level). With the piano synth, this opt can only have the effect of controlling the amp within the natural duration of the note and can not prolong the sound.",
            :validations => [v_positive(:decay)],
            :modulatable => false,
            :bpm_scale => true
          },

          :sustain =>
          {
            :doc => "Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. With the piano synth, this opt can only have the effect of controlling the amp within the natural duration of the note and can not prolong the sound.",
            :validations => [v_positive(:sustain)],
            :modulatable => false,
            :bpm_scale => true
          },

          :release =>
          {
            :doc => "Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. With the piano synth, this opt can only have the effect of controlling the amp within the natural duration of the note and can not prolong the sound.",
            :validations => [v_positive(:release)],
            :modulatable => false,
            :bpm_scale => true
          }


        }

      end
    end

    class SynthKalimba < SonicPiSynth
      def name
        "SynthKalimba"
      end

      def introduced
        Version.new(3,3,0)
      end

      def synth_name
        "kalimba"
      end

      def doc
        "A synthesised kalimba (a type of African thumb piano). Note that due to the plucked nature of this synth the envelope opts such as `attack:`, `sustain:` and `release:` do not work as expected. They can only shorten the natural length of the note, not prolong it. Note the default envelope is longer than usual - sustain: 4 and release: 1"
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,
          :attack => 0,
          :decay => 0,
          :sustain => 4,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :clickiness => 0.1
        }
      end

      def specific_arg_info
        {
          :clickiness => {
            :doc => "Ratio of percussive click to melodic note in the sound. A low clickiness like 0.1 works well - higher values might give the impression that the instrument is being played harder. Very high values (towards 1) will be louder!",
            :validations => [v_between_inclusive(:clickiness, 0, 1)],
            :modulatable => false,
            :bpm_scale => false
          },
          :attack =>
          {
            :doc => "Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. With the kalimba synth, this opt can only have the effect of shortening the attack phase, not prolonging it.",
            :validations => [v_positive(:attack)],
            :modulatable => false,
            :bpm_scale => true
          },

          :decay =>
          {
            :doc => "Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level). With the kalimba synth, this opt can only have the effect of controlling the amp within the natural duration of the note and can not prolong the sound.",
            :validations => [v_positive(:decay)],
            :modulatable => false,
            :bpm_scale => true
          },

          :sustain =>
          {
            :doc => "Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. With the kalimba synth, this opt can only have the effect of controlling the amp within the natural duration of the note and can not prolong the sound.",
            :validations => [v_positive(:sustain)],
            :modulatable => false,
            :bpm_scale => true
          },

          :release =>
          {
            :doc => "Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. With the kalimba synth, this opt can only have the effect of controlling the amp within the natural duration of the note and can not prolong the sound.",
            :validations => [v_positive(:release)],
            :modulatable => false,
            :bpm_scale => true
          }
        }
      end

    end

    class SynthRodeo < SonicPiSynth
      def name
        "SynthRodeo"
      end

      def introduced
        Version.new(3,3,0)
      end

      def synth_name
        "rodeo"
      end

      def doc
        "Classic 70's electric piano sound, with built-in compressor and chorus."
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,
          :attack => 0,
          :decay => 1,
          :sustain => 0.8,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :use_chorus => 1,
          :use_compressor => 1,
          :cutoff => 72,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0
        }
      end

      def specific_arg_info
        {
          :use_compressor =>
          {
            :doc => "Enable the compressor (on by default).",
            :validations => [v_one_of(:use_compressor, [0, 1])],
            :modulatable => false
          },
          :use_chorus =>
          {
            :doc => "Enable the chorus effect (on by default).",
            :validations => [v_one_of(:use_chorus, [0, 1])],
            :modulatable => false
          }
        }
      end

    end

    class SynthPiano < SonicPiSynth
      def name
        "SynthPiano"
      end

      def introduced
        Version.new(2,6,0)
      end

      def synth_name
        "piano"
      end

      def doc
        "A basic piano synthesiser. Note that due to the plucked nature of this synth the envelope opts such as `attack:`, `sustain:` and `release:` do not work as expected. They can only shorten the natural length of the note, not prolong it."
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,
          :vel => 0.2,
          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :hard => 0.5,
          :stereo_width => 0,
        }
      end

      def specific_arg_info
        {
          :note =>
          {
            :doc => "Note to play. Either a MIDI number or a symbol representing a note. For example: `30`, `52`, 56.5, `:C`, `:C2`, `:Eb4`, or `:Ds3`.",
            :validations => [v_positive(:note), v_less_than(:note, 231)],
            :modulatable => true
          },

          :vel => {
            :doc => "Velocity of keypress. ",
            :validations => [v_between_inclusive(:vel, 0, 1)],
            :modulatable => false},

          :hard => {
            :doc => "Hardness of keypress. ",
           :validations => [v_between_inclusive(:hard, 0, 1)],
            :modulatable => false},

          :stereo_width => {
            :doc => "Width of the stereo effect (which makes low notes sound towards the left, high notes towards the right). 0 to 1.",
            :validations => [v_between_inclusive(:stereo_width, 0, 1)],
            :modulatable => false},

          :attack =>
          {
            :doc => "Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. With the piano synth, this opt can only have the effect of shortening the attack phase, not prolonging it.",
            :validations => [v_positive(:attack)],
            :modulatable => false,
            :bpm_scale => true
          },

          :decay =>
          {
            :doc => "Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level). With the piano synth, this opt can only have the effect of controlling the amp within the natural duration of the note and can not prolong the sound.",
            :validations => [v_positive(:decay)],
            :modulatable => false,
            :bpm_scale => true
          },

          :sustain =>
          {
            :doc => "Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. With the piano synth, this opt can only have the effect of controlling the amp within the natural duration of the note and can not prolong the sound.",
            :validations => [v_positive(:sustain)],
            :modulatable => false,
            :bpm_scale => true
          },

          :release =>
          {
            :doc => "Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. With the piano synth, this opt can only have the effect of controlling the amp within the natural duration of the note and can not prolong the sound.",
            :validations => [v_positive(:release)],
            :modulatable => false,
            :bpm_scale => true
          }


        }

      end
    end

    class Growl < SonicPiSynth
      def name
        "Growl"
      end

      def introduced
        Version.new(2,4,0)
      end

      def synth_name
        "growl"
      end

      def doc
        "A deep rumbling growl with a bright sine shining through at higher notes."
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,

          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,

          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0.1,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2,

          :cutoff => 130,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,
          :res => 0.7,
          :res_slide => 0,
          :res_slide_shape => 1,
          :res_slide_curve => 0,
        }
      end
    end

    class DarkAmbience < SonicPiSynth
      def name
        "Dark Ambience"
      end

      def introduced
        Version.new(2,4,0)
      end

      def synth_name
        "dark_ambience"
      end

      def doc
        "A slow rolling bass with a sparkle of light trying to escape the darkness. Great for an ambient sound."
      end

      def arg_defaults
        { :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2,

          :cutoff => 110,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,
          :res => 0.7,
          :res_slide => 0,
          :res_slide_shape => 1,
          :res_slide_curve => 0,

          :detune1 => 12,
          :detune1_slide => 0,
          :detune1_slide_shape => 1,
          :detune1_slide_curve => 0,

          :detune2 => 24,
          :detune2_slide => 0,
          :detune2_slide_shape => 1,
          :detune2_slide_curve => 0,

          :noise => 0,
          :ring => 0.2,
          :room => 70,
          :reverb_time => 100
        }
      end

      def specific_arg_info
        {
          :ring => {
            :doc => "Amount of ring in the sound. Lower values create a more rough sound, higher values produce a sound with more focus.",
            :validations => [v_between_inclusive(:ring, 0.1, 50)],
            :modulatable => true
          },
          :room =>
          {
            :doc => "Room size in squared metres used to calculate the reverb.",
            :validations => [v_greater_than_oet(:room, 0.1), v_less_than_oet(:room, 300)],
            :modulatable => false
          },
          :reverb_time =>
          {
            :doc => "How long in beats the reverb should go on for.",
            :validations => [v_positive(:reverb_time)],
            :modulatable => false
          },
          :detune1 =>
          {
            :doc => "Distance (in MIDI notes) between the main note and the second component of sound. Affects thickness, sense of tuning and harmony.",
          },


          :detune1_slide =>
          {
            :doc => generic_slide_doc(:detune1),
            :validations => [v_positive(:detune1_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :detune2 =>
          {
            :doc => "Distance (in MIDI notes) between the main note and the third component of sound. Affects thickness, sense of tuning and harmony. Tiny values such as 0.1 create a thick sound.",
          },

          :detune2_slide =>
          {
            :doc => generic_slide_doc(:detune2),
            :validations => [v_positive(:detune2_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :noise =>
          { :doc => "Noise source. Has a subtle effect on the timbre of the sound. 0=pink noise (the default), 1=brown noise, 2=white noise, 3=clip noise and 4=grey noise",
            :validations => [v_one_of(:noise, [0, 1, 2, 3, 4])],
            :modulatable => true
          }

        }
      end
    end

    class DarkSeaHorn < SonicPiSynth
      def name
        "Dark Sea Horn"
      end

      def introduced
        Version.new(2,4,0)
      end

      def synth_name
        "dark_sea_horn"
      end

      def doc
        "A deep, rolling sea horn echoing across the empty water."
      end

      def arg_defaults
        {:note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,

          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,

          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 1,
          :decay => 0,
          :sustain => 0,
          :release => 4.0,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2
        }
      end
    end

    class Singer < SonicPiSynth
      def name
        "Singer"
      end

      def introduced
        Version.new(2,4,0)
      end

      def synth_name
        "singer"
      end

      def doc
        "Simulating the sound of a vibrato human singer.

       #Bass
       singer note: :G2

       #Tenor
       singer note: :C#4

       #Alto
       singer note: :F#4

       #Soprano
       singer note: :D5"
      end

      def arg_defaults
        {:note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,

          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,

          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 1,
          :decay => 0,
          :sustain => 0,
          :release => 4.0,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2
        }
      end

      def specific_arg_info
        {
          :vibrato_speed =>
          {
            :doc => "How fast the singer switches between two notes."
          },
          :vibrato_depth =>
          {
            :doc => "How far the singer travels between notes."
          }
        }
      end
    end

    class Hollow < SonicPiSynth
      def name
        "Hollow"
      end

      def introduced
        Version.new(2,4,0)
      end

      def synth_name
        "hollow"
      end

      def doc
        "A hollow breathy sound constructed from random noise"
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,

          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,

          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2,

          :cutoff => 90,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,

          :res => 0.99,
          :res_slide => 0,
          :res_slide_shape => 1,
          :res_slide_curve => 0,

          :noise => 1,
          :norm => 0

        }
      end

      def specific_arg_info
        {
          :norm =>
          {
            :doc => "Normalise the audio (make quieter parts of the synth's sound louder and louder parts quieter) - this is similar to the normaliser FX. This may emphasise any clicks caused by clipping.",
            :validations => [v_one_of(:norm, [0, 1])],
            :modulatable => true
          },

          :res =>
          {
            :doc => "Filter resonance as a value between 0 and 1. Only functional if a cutoff value is specified. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.",
            :validations => [v_positive(:res), v_less_than(:res, 1)],
            :modulatable => true
          },

          :noise =>
          { :doc => "Noise source. Has a subtle effect on the timbre of the sound. 0=pink noise, 1=brown noise (the default), 2=white noise, 3=clip noise and 4=grey noise",
            :validations => [v_one_of(:noise, [0, 1, 2, 3, 4])],
            :modulatable => true
          }
        }
      end
    end

    class Zawa < SonicPiSynth
      def name
        "Zawa"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "zawa"
      end

      def doc
        "Saw wave with oscillating timbre. Produces moving saw waves with a unique character controllable with the control oscillator (usage similar to mod synths)."
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,

          :cutoff => 100,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,
          :res => 0.9,
          :res_slide => 0,
          :res_slide_shape => 1,
          :res_slide_curve => 0,

          :phase => 1,
          :phase_slide => 0,
          :phase_slide_shape => 1,
          :phase_slide_curve => 0,
          :phase_offset => 0,

          :wave => 3,
          :invert_wave => 0,
          :range => 24,
          :range_slide => 0,
          :range_slide_shape => 1,
          :range_slide_curve => 0,
          :disable_wave => 0,
          :pulse_width => 0.5,
          :pulse_width_slide => 0,
          :pulse_width_slide_shape => 1,
          :pulse_width_slide_curve => 0,

        }
      end

      def specific_arg_info
        {
          :phase =>
          {
            :doc => "Phase duration in beats of timbre modulation.",
            :validations => [v_positive_not_zero(:phase)],
            :modulatable => true,
            :bpm_scale => true
          },


          :phase_slide =>
          {
            :doc => generic_slide_doc(:phase),
            :validations => [v_positive(:phase_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :depth_slide =>
          {
            :doc => generic_slide_doc(:depth),
            :validations => [v_positive(:depth_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :phase_offset =>
          {
            :doc => "Initial phase offset of the sync wave (a value between 0 and 1).",
            :validations => [v_between_inclusive(:phase_offset, 0, 1)],
            :modulatable => false
          },

          :range =>
          {
            :doc => "Range of the associated sync saw in MIDI notes from the main note. Modifies timbre.",
            :validations => [v_between_inclusive(:range, 0, 90)],
            :modulatable => true
          },

          :range_slide =>
          {
            :doc => generic_slide_doc(:range),
            :validations => [v_positive(:range_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :wave =>
          {
            :doc => "Wave shape controlling freq sync saw wave. 0=saw wave, 1=pulse, 2=triangle wave and 3=sine wave.",
            :validations => [v_one_of(:wave, [0, 1, 2, 3])],
            :modulatable => true
          },

          :invert_wave =>
          {
            :doc => "Invert sync freq control waveform (i.e. flip it on the y axis). 0=uninverted wave, 1=inverted wave.",
            :validations => [v_one_of(:invert_wave, [0, 1])],
            :modulatable => true
          },

          :disable_wave =>
          {
            :doc => "Enable and disable sync control wave (setting to 1 will stop timbre movement).",
            :validations => [v_one_of(:disable_wave, [0, 1])],
            :modulatable => true
          }
        }
      end
    end

    class Prophet < SonicPiSynth
      def name
        "The Prophet"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "prophet"
      end

      def doc
        "Dark and swirly, this synth uses Pulse Width Modulation (PWM) to create a timbre which continually moves around. This effect is created using the pulse ugen which produces a variable width square wave. We then control the width of the pulses using a variety of LFOs - sin-osc and lf-tri in this case. We use a number of these LFO modulated pulse ugens with varying LFO type and rate (and phase in some cases) to provide the LFO with a different starting point. We then mix all these pulses together to create a thick sound and then feed it through a resonant low pass filter (rlpf). For extra bass, one of the pulses is an octave lower (half the frequency) and its LFO has a little bit of randomisation thrown into its frequency component for that extra bit of variety.

Synth design adapted from:
The Prophet Speaks (page 2)
Steal This Sound,  Mitchell Sigman"
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2,

          :cutoff => 110,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,
          :res => 0.7,
          :res_slide => 0,
          :res_slide_shape => 1,
          :res_slide_curve => 0,
        }
      end

    end

    class ChipLead < SonicPiSynth
      def name
        "Chip Lead"
      end

      def introduced
        Version.new(2,10,0)
      end

      def synth_name
        "chiplead"
      end

      def doc
        "A slightly clipped square (pulse) wave with phases of 12.5%, 25% or 50% modelled after the 2A03 chip found in voices 1 and 2 of the NES games console. This can be used for retro sounding leads and harmonised lines. This also adds an opt 'note_resolution' which locks the note slide to certain pitches which are multiples of the step size. This allows for emulation of the sweep setting on the 2A03."
      end

      def arg_defaults
        {
          :note => 60,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :note_resolution => 0.1,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2,

          :width => 0
        }
      end

      def specific_arg_info
        {
          :width =>
          {
            :doc => "Which of the three pulse_widths to use - 0 => 12.5%, 1 => 25%, 2 => 50%",
            :validations => [v_one_of(:width, [0, 1, 2])],
            :modulatable => true,
          },

          :note_resolution =>
          {
            :doc => "Locks down the note resolution to be multiples of this (MIDI) number. For example, a `note_resolution:` of 1 will only allow semitones to be played. When used in conjunction with `note_slide:` produces a staircase of notes rather than a continuous line which is how things were on the NES. Set to 0 to disable. This wasn't a feature of this triangle (bass) channel on the original chip but some emulators have added it in since.",
            :validations => [v_positive(:note_resolution)],
            :modulatable => true
          },
        }
      end
    end

    class ChipBass < SonicPiSynth
      def name
        "Chip Bass"
      end

      def introduced
        Version.new(2,10,0)
      end

      def synth_name
        "chipbass"
      end

      def doc
        "A 16 step triangle wave modelled after the 2A03 chip found in voice 3 of the NES games console. This can be used for retro sounding basslines. For complete authenticity with the 2A03 bear in mind that the triangle channel on that chip didn't have a volume control."
      end

      def arg_defaults
        {
          :note => 60,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :note_resolution => 0.1,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2,
        }
      end

      def specific_arg_info
        {
          :note_resolution =>
          {
            :doc => "Locks down the note resolution to be multiples of this (MIDI) number. For example, a `note_resolution:` of 1 will only allow semitones to be played. When used in conjunction with `note_slide:` produces a staircase of notes rather than a continuous line which is how things were on the NES. Set to 0 to disable. This wasn't a feature of this triangle (bass) channel on the original chip but some emulators have added it in since.",
            :validations => [v_positive(:note_resolution)],
            :modulatable => true
          },
        }
      end
    end

    class Pitchless < SonicPiSynth
    end

    class Noise < Pitchless
      def name
        "Noise"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "noise"
      end

      def doc
        "Noise that contains equal amounts of energy at every frequency - comparable to radio static. Useful for generating percussive sounds such as snares and hand claps. Also useful for simulating wind or sea effects."
      end

      def arg_defaults
        {
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2,

          :cutoff => 110,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,
          :res => 0,
          :res_slide => 0,
          :res_slide_shape => 1,
          :res_slide_curve => 0,
        }
      end

    end

    class GNoise < Noise
      def name
        "Grey Noise"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "gnoise"
      end

      def doc
        "Generates noise which results from flipping random bits in a word. The spectrum is emphasised towards lower frequencies. Useful for generating percussive sounds such as snares and hand claps. Also useful for simulating wind or sea effects."
      end
    end

    class BNoise < Noise
      def name
        "Brown Noise"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "bnoise"
      end

      def doc
        "Noise whose spectrum falls off in power by 6 dB per octave. Useful for generating percussive sounds such as snares and hand claps. Also useful for simulating wind or sea effects."
      end

    end

    class PNoise < Noise
      def name
        "Pink Noise"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "pnoise"
      end

      def doc
        "Noise whose spectrum falls off in power by 3 dB per octave. Useful for generating percussive sounds such as snares and hand claps. Also useful for simulating wind or sea effects."
      end

    end

    class CNoise < Noise
      def name
        "Clip Noise"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "cnoise"
      end

      def doc
        "Generates noise whose values are either -1 or 1. This produces the maximum energy for the least peak to peak amplitude. Useful for generating percussive sounds such as snares and hand claps. Also useful for simulating wind or sea effects."
      end

    end

    class ChipNoise < Noise
      def name
        "Chip Noise"
      end

      def introduced
        Version.new(2,10,0)
      end

      def synth_name
        "chipnoise"
      end

      def doc
        "Generates noise whose values are either -1 or 1 (like a pulse or square wave) with one of 16 particular frequencies. This is similar to the noise channel on the 2A03 chip used in the NES games console, although it lacks the same Pseudo-Random Number Generator (PRNG) and doesn't implement the 2A03's lesser used noise mode. The amplitude envelope defaults to moving by step to keep that 16 bit feel and this synth also has a slight soft clipping to better imitate the original sound of the device. Use for retro effects, hand claps, snare drums and hi-hats."
      end

      def arg_defaults
        {
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 0,
          :amp_slide_curve => 1,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 0,

          :freq_band => 0,
          :freq_band_slide => 0,
          :freq_band_slide_shape => 1,
          :freq_band_slide_curve => 0,
        }
      end

      def specific_arg_info
        {
          :freq_band =>
          {
            :doc => "Which of the 16 frequency bands to use, from 0 to 15. These range from 220Hz to 225kHz as on the original chip. This arg will accept floats but round to the nearest integer to allow for sweeping through the 16 set points with envelopes.",
            :validations => [v_between_inclusive(:freq_band, 0, 15)],
            :modulatable => true,
          },

          :freq_band_slide =>
          {
            :doc => generic_slide_doc(:freq_band),
            :validations => [v_positive(:freq_band_slide)],
            :modulatable => true,
            :bpm_scale => true
          },
        }
      end
    end

    class TechSaws < SonicPiSynth
      def name
        "TechSaws"
      end

      def introduced
        Version.new(2,11,0)
      end

      def synth_name
        "tech_saws"
      end

      def doc
        "Slightly modified supersaw implementation based on http://sccode.org/1-4YS"
      end

      def arg_defaults
        {
          :note => 52,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,

          :attack => 0,
          :decay => 0,
          :sustain => 0,
          :release => 1,
          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2,

          :cutoff => 130,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,
          :res => 0.7,
          :res_slide => 0,
          :res_slide_shape => 1,
          :res_slide_curve => 0,

        }
      end
    end

    class StudioInfo < SonicPiSynth
      def user_facing?
        false
      end
    end


    class BasicMonoPlayer < StudioInfo
      def name
        "Basic Mono Sample Player (no env)"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "basic_mono_player"
      end

      def doc
        ""
      end

      def munge_opts(studio, args_h)
        alias_opts!(:cutoff, :lpf, args_h)
        alias_opts!(:cutoff_slide, :lpf_slide, args_h)
        alias_opts!(:cutoff_slide_curve, :lpf_slide_curve, args_h)
        alias_opts!(:cutoff_slide_shape, :lpf_slide_shape, args_h)
        args_h
      end

      def arg_defaults
        {
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,
          :rate => 1,
          :lpf => -1,
          :lpf_slide => 0,
          :lpf_slide_shape => 1,
          :lpf_slide_curve => 0,
          :hpf => -1,
          :hpf_slide => 0,
          :hpf_slide_shape => 1,
          :hpf_slide_curve => 0
        }
      end

      def specific_arg_info
        {
          :rate =>
          {
            :validations => [v_not_zero(:rate)],
            :modulatable => false
          },


          :lpf =>
          {
            :doc => "Low pass filter cutoff value. A MIDI note representing the highest frequencies allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.",
            :validations => [v_positive(:lpf), v_less_than(:lpf, 131)],
            :modulatable => true,
            :midi => true
          },

          :hpf =>
          {
            :doc => "High pass filter cutoff value. A MIDI note representing the lowest frequencies allowed to be present in the sound. A high value like 100 makes the sound thin and whispy, a low value like 40 removes just the lower bass components of the sound.",
            :validations => [v_positive(:hpf), v_less_than(:hpf, 119)],
            :modulatable => true,
            :midi => true
          }
        }

      end
    end

    class BasicStereoPlayer < BasicMonoPlayer
      def name
        "Basic Stereo Sample Player (no env)"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "basic_stereo_player"
      end

      def doc
        ""
      end
    end

    class MonoPlayer < BasicMonoPlayer
      def name
        "Mono Sample Player"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "mono_player"
      end

      def doc
        ""
      end

      def arg_defaults
        {
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :pre_amp => 1,
          :pre_amp_slide => 0,
          :pre_amp_slide_shape => 1,
          :pre_amp_slide_curve => 0,
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,


          :attack => 0,
          :decay => 0,
          :sustain => -1,
          :release => 0,

          :lpf => -1,
          :lpf_slide => 0,
          :lpf_slide_shape => 1,
          :lpf_slide_curve => 0,
          :lpf_attack => 0,
          :lpf_decay => 0,
          :lpf_sustain => -1,
          :lpf_release => 0,
          :lpf_init_level => -1,
          :lpf_attack_level => -1,
          :lpf_decay_level => -1,
          :lpf_sustain_level => -1,
          :lpf_release_level => -1,
          :lpf_env_curve => 2,
          :lpf_min => -1,
          :lpf_min_slide => 0,
          :lpf_min_slide_shape => 1,
          :lpf_min_slide_curve => 0,

          :hpf => -1,
          :hpf_slide => 0,
          :hpf_slide_shape => 1,
          :hpf_slide_curve => 0,
          :hpf_attack => 0,
          :hpf_sustain => -1,
          :hpf_decay => 0,
          :hpf_release => 0,
          :hpf_init_level => -1,
          :hpf_attack_level => -1,
          :hpf_decay_level => -1,
          :hpf_sustain_level => -1,
          :hpf_release_level => -1,
          :hpf_env_curve => 2,
          :hpf_max => -1,
          :hpf_max_slide => 0,
          :hpf_max_slide_shape => 1,
          :hpf_max_slide_curve => 0,


          :attack_level => 1,
          :decay_level => :sustain_level,
          :sustain_level => 1,
          :env_curve => 2,


          :rate => 1,
          :start => 0,
          :finish => 1,


          :norm => 0,

          :pitch => 0,
          :pitch_slide => 0,
          :pitch_slide_shape => 1,
          :pitch_slide_curve => 0,
          :window_size => 0.2,
          :window_size_slide => 0,
          :window_size_slide_shape => 1,
          :window_size_slide_curve => 0,
          :pitch_dis => 0.0,
          :pitch_dis_slide => 0,
          :pitch_dis_slide_shape => 1,
          :pitch_dis_slide_curve => 0,
          :time_dis => 0.0,
          :time_dis_slide => 0,
          :time_dis_slide_shape => 1,
          :time_dis_slide_curve => 0,

          :compress => 0,
          :threshold => 0.2,
          :threshold_slide => 0,
          :threshold_slide_shape => 1,
          :threshold_slide_curve => 0,
          :clamp_time => 0.01,
          :clamp_time_slide => 0,
          :clamp_time_slide_shape => 1,
          :clamp_time_slide_curve => 0,
          :slope_above => 0.5,
          :slope_above_slide => 0,
          :slope_above_slide_shape => 1,
          :slope_above_slide_curve => 0,
          :slope_below => 1,
          :slope_below_slide => 0,
          :slope_below_slide_shape => 1,
          :slope_below_slide_curve => 0,
          :relax_time => 0.01,
          :relax_time_slide => 0,
          :relax_time_slide_shape => 1,
          :relax_time_slide_curve => 0
        }
      end

      def specific_arg_info
        super.merge({

          :attack =>
          {
            :doc => "Duration of the attack phase of the envelope.",
            :validations => [v_positive(:attack)],
            :modulatable => false,
            :default => 0
          },

          :decay =>
          {
            :doc => "Duration of the decay phase of the envelope.",
            :validations => [v_positive(:decay)],
            :modulatable => false,
            :default => 0
          },

          :sustain =>
          {
            :doc => "Duration of the sustain phase of the envelope. When -1 (the default) will auto-stretch.",
            :validations => [[lambda{|args| v = args[:sustain] ; (v == -1) || (v >= 0)}, "must either be a positive value or -1"]],

            :modulatable => false,
            :default => -1
          },

          :release =>
          {
            :doc => "Duration of the release phase of the envelope.",
            :validations => [v_positive(:release)],
            :modulatable => false,
            :default => 0
          },


          :rate =>
          {
            :doc => "Rate with which to play back - default is 1. Playing the sample at rate 2 will play it back at double the normal speed. This will have the effect of doubling the frequencies in the sample and halving the playback time. Use rates lower than 1 to slow the sample down. Negative rates will play the sample in reverse.",
            :validations => [v_not_zero(:rate)],
            :modulatable => false,
            :default => 1
          },

          :start =>
          {
            :doc => "A fraction (between 0 and 1) representing where in the sample to start playback. 1 represents the end of the sample, 0.5 half-way through etc.",
            :validations => [v_between_inclusive(:start, 0, 1)],
            :modulatable => false,
            :default => 0
          },

          :finish =>
          {
            :doc => "A fraction (between 0 and 1) representing where in the sample to finish playback. 1 represents the end of the sample, 0.5 half-way through etc.",
            :validations => [v_between_inclusive(:finish, 0, 1)],
            :modulatable => false,
            :default => 1
          },

          :norm =>
          {
            :doc => "Normalise the audio (make quieter parts of the sample louder and louder parts quieter) - this is similar to the normaliser FX. This may emphasise any clicks caused by clipping.",
            :validations => [v_one_of(:norm, [0, 1])],
            :modulatable => true,
            :default => 0
          },

          :window_size =>
          {
            :doc => "Pitch shift works by chopping the input into tiny slices, then playing these slices at a higher or lower rate. If we make the slices small enough and overlap them, it sounds like the original sound with the pitch changed.

  The window_size is the length of the slices and is measured in seconds. It needs to be around 0.2 (200ms) or greater for pitched sounds like guitar or bass, and needs to be around 0.02 (20ms) or lower for percussive sounds like drum loops. You can experiment with this to get the best sound for your input.",
            :validations => [v_greater_than(:window_size, 0.00005)],
            :modulatable => true,
            :default => 0.2
          },

          :pitch =>
          {
            :doc => "Pitch adjustment in semitones. 1 is up a semitone, 12 is up an octave, -12 is down an octave etc. Maximum upper limit of 24 (up 2 octaves). Lower limit of -72 (down 6 octaves). Decimal numbers can be used for fine tuning.",
            :validations => [v_greater_than_oet(:pitch, -72), v_less_than_oet(:pitch, 24)],
            :modulatable => true,
            :default => 0
          },

          :pitch_dis =>
          {
            :doc => "Pitch dispersion - how much random variation in pitch to add. Using a low value like 0.001 can help to \"soften up\" the metallic sounds, especially on drum loops. To be really technical, pitch_dispersion is the maximum random deviation of the pitch from the pitch ratio (which is set by the pitch param)",
            :validations => [v_greater_than_oet(:pitch_dis, 0)],
            :modulatable => true
          },

          :time_dis =>
          {
            :doc => "Time dispersion - how much random delay before playing each grain (measured in seconds). Again, low values here like 0.001 can help to soften up metallic sounds introduced by the effect. Large values are also fun as they can make soundscapes and textures from the input, although you will most likely lose the rhythm of the original. NB - This won't have an effect if it's larger than window_size.",
            :validations => [v_greater_than_oet(:time_dis, 0)],
            :modulatable => true
          },

          :lpf_init_level =>
          {
            :doc => "The initial low pass filter envelope value as a MIDI note. This envelope is bypassed if no lpf env opts are specified. Default value is to match the `lpf_min:` opt.",
            :validations => [v_between_inclusive(:lpf_init_level, 0, 130)],
            :default => "lpf_min",
            :modulatable => false,
            :midi => true
          },

          :lpf_attack_level =>
          {
            :doc => "The peak low pass filter envelope value after the attack phase as a MIDI note. This envelope is bypassed if no lpf env opts are specified. Default value is match the `lpf_decay_level:` opt.",
            :validations => [v_between_inclusive(:lpf_attack_level, 0, 130)],
            :modulatable => false,
            :default => "lpf_decay_level",
            :midi => true
          },

          :lpf_decay_level =>
          {
            :doc => "The level of the low pass filter envelope after the decay phase as a MIDI note. This envelope is bypassed if no lpf env opts are specified. Default value is to match the `lpf_sustain_level:` opt.",
            :validations => [v_between_inclusive(:lpf_decay_level, 0, 130)],
            :modulatable => false,
            :default => "lpf_sustain_level",
            :midi => true
          },

          :lpf_sustain_level =>
          {
            :doc => "The level of the low pass filter envelope after the sustain phase as a MIDI note. This envelope is bypassed if no lpf env opts are specified. Default value is to match the `lpf_release_level:` opt.",
            :validations => [v_between_inclusive(:lpf_sustain_level, 0, 130)],
            :modulatable => false,
            :default => "lpf_release_level",
            :midi => true
          },

          :lpf_release_level =>
          {
            :doc => "The final value of the low pass filter envelope as a MIDI note. This envelope is bypassed if no lpf env opts are specified. Default value is to match the `lpf:` opt.",
            :validations => [v_between_inclusive(:lpf_release_level, 0, 130)],
            :modulatable => false,
            :default => "lpf",
            :midi => true
          },

          :lpf_attack =>
          {
            :doc => "Attack time for low pass filter envelope. Amount of time (in beats) for sound to reach attack_level value. This envelope is bypassed if no lpf env opts are specified.  Default value is set to match amp envelope's attack value.",
            :validations => [v_positive(:lpf_attack)],
            :modulatable => false,
            :default => "attack"
          },

          :lpf_decay =>
          {
            :doc => "Decay time for low pass filter envelope. Amount of time (in beats) for sound to move from `lpf_attack_level:` to the `lpf_sustain_level:`. This envelope is bypassed if no lpf env opts are specified.  Default value is set to match amp envelope's decay value.",
            :validations => [v_positive(:lpf_decay)],
            :modulatable => false,
            :default => "decay"
          },

          :lpf_sustain =>
          {
            :doc => "Amount of time for low pass filter envelope value to remain at sustain level in beats. This envelope is bypassed if no lpf env opts are specified.  When -1 (the default) will auto-stretch.",
            :validations => [[lambda{|args| v = args[:lpf_sustain] ; (v == -1) || (v >= 0)}, "must either be a positive value or -1"]],
            :modulatable => false,
            :default => "sustain"
          },

          :lpf_release =>
          {
            :doc => "Amount of time (in beats) for sound to move from `lpf_sustain_level:` to `lpf_release_level:`. This envelope is bypassed if no lpf env opts are specified. ",
            :validations => [v_positive(:lpf_release)],
            :modulatable => false,
            :default => "release"
          },

          :lpf_env_curve =>
          {
            :doc => "Select the shape of the curve between levels in the cutoff envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed",
            :validations => [v_one_of(:lpf_env_curve, [1, 2, 3, 4, 6, 7])],
            :modulatable => false
          },

          :lpf_min =>
          {
            :doc => "The minimum low pass filter value.",
            :validations => [v_less_than_oet(:lpf_min, 130)],
            :modulatable => true,
            :default => 130,
            :midi => true
          },

          :lpf_min_slide =>
          {
            :doc => generic_slide_doc(:lpf_min),
            :validations => [v_positive(:lpf_min_slide)],
            :modulatable => true,
            :bpm_scale => true
                      },

          :hpf_init_level =>
          {
            :doc => "The initial high pass filter envelope value as a MIDI note. This envelope is bypassed if no hpf env opts are specified. Default value is set to 130",
            :validations => [v_between_inclusive(:hpf_init_level, 0, 130)],
            :modulatable => false,
            :default => 130,
            :midi => true
          },

          :hpf_attack_level =>
          {
            :doc => "The peak hpf cutoff (value of hpf cutoff at peak of attack) as a MIDI note.",
            :validations => [v_between_inclusive(:hpf_attack_level, 0, 130)],
            :modulatable => false,
            :midi => true,
            :default => "hpf_decay_level"
          },

          :hpf_decay_level =>
          {
            :doc => "The level of hpf cutoff after the decay phase as a MIDI note.",
            :validations => [v_between_inclusive(:hpf_decay_level, 0, 130)],
            :modulatable => false,
            :midi => true,
            :default => "hpf_sustain_level"

          },


          :hpf_sustain_level =>
          {
            :doc => "The sustain hpf cutoff (value of hpf cutoff at sustain time) as a MIDI note.",
            :validations => [v_between_inclusive(:hpf_sustain_level, 0, 130)],
            :modulatable => false,
            :midi => true,
            :default => "hpf_release_level"
          },

          :hpf_release_level =>
          {
            :doc => "The sustain hpf cutoff (value of hpf cutoff at sustain time) as a MIDI note.",
            :validations => [v_between_inclusive(:hpf_release_level, 0, 130)],
            :modulatable => false,
            :midi => true,
            :default => "hpf"
          },


          :hpf_attack =>
          {
            :doc => "Attack time for hpf cutoff filter. Amount of time (in beats) for sound to reach full hpf cutoff value. Default value is set to match amp envelope's attack value.",
            :validations => [v_positive(:hpf_attack)],
            :modulatable => false,
            :default => "attack"
          },

          :hpf_decay =>
          {
            :doc => "Decay time for hpf cutoff filter. Amount of time (in beats) for sound to move from full hpf cutoff value (cutoff attack level) to the hpf cutoff sustain level. Default value is set to match amp envelope's decay value.",
            :validations => [v_positive(:hpf_decay)],
            :modulatable => false,
            :default => "decay"
          },

          :hpf_sustain =>
          {
            :doc => "Amount of time for hpf cutoff value to remain at hpf sustain level in beats. When -1 (the default) will auto-stretch.",
            :validations => [[lambda{|args| v = args[:hpf_sustain] ; (v == -1) || (v >= 0)}, "must either be a positive value or -1"]],
            :modulatable => false,
            :default => "sustain"
          },

          :hpf_release =>
          {
            :doc => "Amount of time (in beats) for sound to move from hpf cutoff sustain value to hpf cutoff min value. Default value is set to match amp envelope's release value.",
            :validations => [v_positive(:hpf_release)],
            :modulatable => false,
            :default => "release"
          },

          :hpf_env_curve =>
          {
            :doc => "Select the shape of the curve between levels in the hpf cutoff envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed",
            :validations => [v_one_of(:hpf_env_curve, [1, 2, 3, 4, 6, 7])],
            :modulatable => false
          },

          :hpf_min =>
          {
            :doc => "The minimum cutoff value.",
            :validations => [v_less_than_oet(:hpf_min, 130)],
            :modulatable => true,
            :midi => true
          },

          :hpf_min_slide =>
          {
            :doc => generic_slide_doc(:hpf_min),
            :validations => [v_positive(:hpf_min_slide)],
            :modulatable => true,
            :bpm_scale => true
          },


          :hpf_max =>
          {
            :doc => "The maximum high pass filter value.",
            :validations => [v_less_than_oet(:hpf_max, 130)],
            :modulatable => true,
            :default => 200,
            :midi => true
          },


          :compress =>
          {
            :doc => "Enable the compressor. This sits at the end of the internal FX chain immediately before the `amp:` opt. Therefore to drive the compressor use the `pre_amp:` opt which will amplify the signal before it hits any internal FX. The compressor compresses the dynamic range of the incoming signal. Equivalent to automatically turning the amp down when the signal gets too loud and then back up again when it's quiet. Useful for ensuring the containing signal doesn't overwhelm other aspects of the sound. Also a general purpose hard-knee dynamic range processor which can be tuned via the opts to both expand and compress the signal.",
            :validations => [v_one_of(:compress, [0, 1])],
            :modulatable => true
          },

          :threshold =>
          {
            :doc => "Threshold value determining the break point between slope_below and slope_above. Only valid if the compressor is enabled by turning on the compress: opt.",
            :validations => [v_positive(:threshold)],
            :modulatable => true
          },

          :threshold_slide =>
          {
            :doc => generic_slide_doc(:threshold),
            :validations => [v_positive(:threshold_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :slope_below =>
          {
            :doc => "Slope of the amplitude curve below the threshold. A value of 1 means that the output of signals with amplitude below the threshold will be unaffected. Greater values will magnify and smaller values will attenuate the signal. Only valid if the compressor is enabled by turning on the compress: opt.",
            :validations => [],
            :modulatable => true
          },

          :slope_below_slide =>
          {
            :doc => generic_slide_doc(:slope_below),
            :validations => [v_positive(:slope_below_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :slope_above =>
          {
            :doc => "Slope of the amplitude curve above the threshold. A value of 1 means that the output of signals with amplitude above the threshold will be unaffected. Greater values will magnify and smaller values will attenuate the signal. Only valid if the compressor is enabled by turning on the compress: opt.",

            :validations => [],
            :modulatable => true
          },

          :slope_above_slide =>
          {
            :doc => generic_slide_doc(:slope_above),
            :validations => [v_positive(:slope_above_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :clamp_time =>
          {
            :doc => "Time taken for the amplitude adjustments to kick in fully (in seconds). This is usually pretty small (not much more than 10 milliseconds). Also known as the time of the attack phase. Only valid if the compressor is enabled by turning on the compress: opt.",
            :validations => [v_positive(:clamp_time)],
            :modulatable => true
          },

          :clamp_time_slide =>
          {
            :doc => generic_slide_doc(:clamp_time),
            :validations => [v_positive(:clamp_time_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :relax_time =>
          {
            :doc => "Time taken for the amplitude adjustments to be released. Usually a little longer than clamp_time. If both times are too short, you can get some (possibly unwanted) artefacts. Also known as the time of the release phase. Only valid if the compressor is enabled by turning on the compress: opt.",
            :validations => [v_positive(:relax_time)],
            :modulatable => true
          },

          :relax_time_slide =>
          {
            :doc => generic_slide_doc(:relax_time),
            :validations => [v_positive(:relax_time_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :pre_amp =>
          {
            :doc => "Amplitude multiplier which takes place immediately before any internal FX such as the low pass filter, compressor or pitch modification. Use this opt if you want to overload the compressor.",
            :validations => [v_positive(:pre_amp)],
            :modulatable => true
          },

          :pre_amp_slide =>
          {
            :doc => generic_slide_doc(:pre_amp),
            :validations => [v_positive(:pre_amp_slide)],
            :modulatable => true,
            :bpm_scale => true
          }

        })
      end

    end

    class StereoPlayer < MonoPlayer
      def name
        "Stereo Sample Player"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "stereo_player"
      end
    end

    class BaseMixer < StudioInfo

    end

    class BasicMixer < BaseMixer
      def name
        "Basic Mixer"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "basic_mixer"
      end

      def arg_defaults
        {
          :amp => 1,
          :amp_slide => 0.1,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0
        }
      end

    end


    class MainMixer < BaseMixer
      def name
        "Main Mixer"
      end

      def introduced
        Version.new(2,8,0)
      end

      def synth_name
        "mixer"
      end

      def arg_defaults
        {
          :amp_slide => 0.02,
          :pre_amp_slide => 0.02,
          :hpf_slide => 0.02,
          :lpf_slide => 0.02,
          :pre_amp => 1,
          :pre_amp_slide_shape => 1,
          :pre_amp_slide_curve => 0,
          :amp => 1,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :hpf => 0,
          :hpf_bypass => 0,
          :hpf_slide_shape => 1,
          :hpf_slide_curve => 0,
          :lpf => 135.5,
          :lpf_bypass => 0,
          :lpf_slide_shape => 1,
          :lpf_slide_curve => 0,
          :force_mono => 0,
          :invert_stereo => 0,
          :limiter_bypass => 0,
          :leak_dc_bypass => 0}

      end

      def default_arg_info
        {}
      end

    end

    class FXInfo < BaseInfo
      def user_facing?
        true
      end

      def trigger_with_logical_clock?
        true
      end

      def prefix
        "sonic-pi-"
      end

      def arg_defaults
        { :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :mix => 1,
          :mix_slide => 0,
          :mix_slide_shape => 1,
          :mix_slide_curve => 0,
          :pre_mix => 1,
          :pre_mix_slide => 0,
          :pre_mix_slide_shape => 1,
          :pre_mix_slide_curve => 0,
          :pre_amp => 1,
          :pre_amp_slide => 0,
          :pre_amp_slide_shape => 1,
          :pre_amp_slide_curve => 0,
        }
      end

      def default_arg_info
        super.merge({
                      :mix =>
                      {
                        :doc => "The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard.",
                        :validations => [v_between_inclusive(:mix, 0, 1)],
                        :modulatable => true
                      },

                      :mix_slide =>
                      {
                        :doc => "Amount of time (in beats) for the mix value to change. A long slide value means that the mix takes a long time to slide from the previous value to the new value. A slide of 0 means that the mix instantly changes to the new value.",
                        :validations => [v_positive(:mix_slide)],
                        :modulatable => true
                      },

                      :pre_mix =>
                      {
                        :doc => "The amount (percentage) of the original signal that is fed into the internal FX system as a value between 0 and 1. With a pre_mix: of 0 the FX is completely bypassed unlike a mix: of 0 where the internal FX is still being fed the original signal but the output of the FX is ignored. The difference between the two is subtle but important and is evident when the FX has a residual component such as echo or reverb. When switching mix: from 0 to 1, the residual component of the FX's output from previous audio is present in the output signal. With pre_mix: there is no residual component of the previous audio in the output signal.",
                        :validations => [v_positive(:pre_mix)],
                        :modulatable => true
                      },

                      :pre_mix_slide =>
                      {
                        :doc => "Amount of time (in beats) for the pre_mix value to change. A long slide value means that the pre_mix takes a long time to slide from the previous value to the new value. A slide of 0 means that the pre_mix instantly changes to the new value.",
                        :validations => [v_positive(:pre_mix_slide)],
                        :modulatable => true
                      },

                      :pre_amp =>
                      {
                        :doc => "Amplification applied to the input signal immediately before it is passed to the FX.",
                        :validations => [v_positive(:pre_amp)],
                        :modulatable => true
                      },

                      :pre_amp_slide =>
                      {
                        :doc => generic_slide_doc(:pre_amp),
                        :validations => [v_positive(:pre_amp_slide)],
                        :modulatable => true
                      },

                      :phase_offset =>
                      {
                        :doc => "Initial modulation phase offset (a value between 0 and 1).",
                        :validations => [v_between_inclusive(:phase_offset, 0, 1)],
                        :modulatable => false
                      },
                    })
      end
    end

    class FXRecord < FXInfo
      def name
        "Record"
      end

      def introduced
        Version.new(3,0,0)
      end

      def synth_name
        "fx_record"
      end

      def trigger_with_logical_clock?
        true
      end

      def doc
        "Recorder!"
      end

      def kill_delay(args_h)
        0
      end

      def on_start(studio, args_h)
        raise "Record FX requires a buffer: opt." unless args_h[:buffer].is_a?(Buffer)
      end

      def on_finish(studio, args_h)
        buf = args_h[:buffer]
        if buf && buf.is_a?(Buffer)
          Thread.new do
            # Run this in a thread otherwise it will block the event
            # system as this method will be called from the event system
            # thread and therefore shouldn't perform synchronous
            # blocking event functions such as the with_done_sync in
            # buffer_alloc triggered by this call to save_buffer!
            studio.save_buffer!(buf, buf.path)
          end
        else
          raise "Record FX completion handler needs a buffer to save - got #{buf.inspect}, #{buf.class} from #{args_h.inspect}"
        end
      end


      def arg_defaults
        { :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
          :mix => 1,
          :mix_slide => 0,
          :mix_slide_shape => 1,
          :mix_slide_curve => 0,
          :pre_mix => 1,
          :pre_mix_slide => 0,
          :pre_mix_slide_shape => 1,
          :pre_mix_slide_curve => 0,
          :pre_amp => 1,
          :pre_amp_slide => 0,
          :pre_amp_slide_shape => 1,
          :pre_amp_slide_curve => 0,
          :buffer => nil
        }
      end

      def default_arg_info
        super.merge({
                      :buffer =>
                      {
                        :doc => "The buffer to record into. Must either be a buffer object, buffer name, list of buffer name and size or the buffer id as a number.",
                        :validations => [v_buffer_like(:buffer)],
                        :modulatable => false,
                        :buffer => true
                      },
                    })
      end
    end

    class FXSoundOut < FXInfo
      def name
        "Sound Out"
      end

      def introduced
        Version.new(3,0,0)
      end

      def synth_name
        "fx_sound_out"
      end

      def trigger_with_logical_clock?
        true
      end

      def doc
        "Outputs a mono signal to a soundcard output of your choice. By default will mix the incoming stereo signal (generated within the FX block) into a single mono channel. However, with the `mode:` opt, it is possible to alternatively send either the incoming left or right channel out directly. "
      end

      def kill_delay(args_h)
        0
      end

      def arg_defaults
        super.merge({
                      :output => 1,
                      :mode => 0
                    })
      end

      def specific_arg_info
        {
          :output =>
          {
            :doc => "Sound card output to send audio to. Indexing starts at 1, so the third output is output 3.",
            :modulatable => true
          },
          :mode =>
          {
            :doc => "Output mixing mode. 0 is a mixed-down mono version of the stereo input, 1 is the left channel only, 2 is the right channel only. ",
            :modulatable => true
          }
        }
      end

    end

    class FXSoundOutStereo < FXInfo
      def name
        "Sound Out Stereo"
      end

      def introduced
        Version.new(3,0,0)
      end

      def synth_name
        "fx_sound_out_stereo"
      end

      def trigger_with_logical_clock?
        true
      end

      def doc
        "Outputs a two-channel stereo signal to two consecutive soundcard outputs of your choice. By default will route the left and right channels of the incoming stereo signal (generated within the FX block) into separate left and right output channels. However, with the `mode:` opt, it is possible to alternatively cross over the channels or mix the incoming stereo channels into a single mono output and duplicate that on both left and right output channels. "

      end

      def kill_delay(args_h)
        0
      end

      def arg_defaults
        super.merge({
                      :output => 1,
                      :mode => 0
                    })
      end

      def specific_arg_info
        {
          :output =>
          {
            :doc => "First of two consecutive sound card outputs to send audio to. Indexing starts at 1 and two outputs are used. Therefore an output of 2 will send audio to both outputs 2 and 3 ",
            :modulatable => true
          },
          :mode =>
          {
            :doc => "Output mixing mode. Mode 0 is standard - left audio on the first channel, right on the second. Mode 1 is inverse - right audio on the first channel, left on the second. Mode 2 is mono - a mixed mono version of both channels is sent to both audio outputs.",
            :modulatable => true
          }
        }
      end
    end


    class FXEQ < FXInfo

      def name
        "EQ"
      end

      def introduced
        Version.new(3,0,0)
      end

      def synth_name
        "fx_eq"
      end

      def trigger_with_logical_clock?
        false
      end

      def doc
        "Basic parametric EQ"
      end

      def arg_defaults
        super.merge({
                      :low_shelf => 0,
                      :low_shelf_slide => 0,
                      :low_shelf_slide_shape => 1,
                      :low_shelf_slide_curve => 0,
                      :low_shelf_note => 43.349957,
                      :low_shelf_note_slide => 0,
                      :low_shelf_note_slide_shape => 1,
                      :low_shelf_note_slide_curve => 0,
                      :low_shelf_slope => 1,
                      :low_shelf_slope_slide => 0,
                      :low_shelf_slope_slide_shape => 1,
                      :low_shelf_slope_slide_curve => 0,

                      :low => 0,
                      :low_slide => 0,
                      :low_slide_shape => 1,
                      :low_slide_curve => 0,
                      :low_note => 59.2130948,
                      :low_note_slide => 0,
                      :low_note_slide_shape => 1,
                      :low_note_slide_curve => 0,
                      :low_q => 0.6,
                      :low_q_slide => 0,
                      :low_q_slide_shape => 1,
                      :low_q_slide_curve => 0,

                      :mid => 0,
                      :mid_slide => 0,
                      :mid_slide_shape => 1,
                      :mid_slide_curve => 0,
                      :mid_note => 83.2130948,
                      :mid_note_slide => 0,
                      :mid_note_slide_shape => 1,
                      :mid_note_slide_curve => 0,
                      :mid_q => 0.6,
                      :mid_q_slide => 0,
                      :mid_q_slide_shape => 1,
                      :mid_q_slide_curve => 0,

                      :high => 0,
                      :high_slide => 0,
                      :high_slide_shape => 1,
                      :high_slide_curve => 0,
                      :high_note => 104.9013539,
                      :high_note_slide => 0,
                      :high_note_slide_shape => 1,
                      :high_note_slide_curve => 0,
                      :high_q => 0.6,
                      :high_q_slide => 0,
                      :high_q_slide_shape => 1,
                      :high_q_slide_curve => 0,

                      :high_shelf => 0,
                      :high_shelf_slide => 0,
                      :high_shelf_slide_shape => 1,
                      :high_shelf_slide_curve => 0,
                      :high_shelf_note => 114.2326448,
                      :high_shelf_note_slide => 0,
                      :high_shelf_note_slide_shape => 1,
                      :high_shelf_note_slide_curve => 0,
                      :high_shelf_slope => 1,
                      :high_shelf_slope_slide => 0,
                      :high_shelf_slope_slide_shape => 1,
                      :high_shelf_slope_slide_curve => 0,
                    })
      end

      def specific_arg_info
        {
          :low_shelf =>
          {
            :doc => "Gain - boost or cut the centre frequency. The low shelf defines the characteristics of the lowest part of the eq FX. A value of 0 will neither boost or cut the low_shelf frequencies. A value of 1 will boost by 15 dB and a value of -1 will cut/attenuate by -15 dB.",
            :modulatable => true
          },

          :low_shelf_slide =>
          {
            :doc => generic_slide_doc(:low_shelf),
            :validations => [v_positive(:low_shelf_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :low_shelf_note =>
          {
            :doc => "Centre frequency of low shelf in MIDI notes.",
            :validations => [v_greater_than(:low_shelf_note, 1)],
            :modulatable => true,
            :midi => true
          },

          :low_shelf_note_slide =>
          {
            :doc => generic_slide_doc(:low_shelf_note),
            :validations => [v_positive(:low_shelf_note_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :low_shelf_slope =>
          {
            :doc => "Low shelf boost/cut slope. When set to 1 (the default), the shelf slope is as steep as it can be and remain monotonically increasing or decreasing gain with frequency.",
            :validations => [v_greater_than_oet(:low_shelf_slope, 0), v_less_than_oet(:low_shelf_slope, 1)],
            :modulatable => true
          },

          :low_shelf_slope_slide =>
          {
            :doc => generic_slide_doc(:low_shelf_slope),
            :validations => [v_positive(:low_shelf_slope_slide)],
            :modulatable => true,
            :bpm_scale => true
          },


          :low =>
          {
            :doc => "Gain - boost or cut the centre frequency of the bass part of the sound. The low shelf defines the characteristics of the bass of the eq FX. A value of 0 will neither boost or cut the bass frequencies. A value of 1 will boost by 15 dB and a value of -1 will cut/attenuate by -15 dB.",
            :modulatable => true
          },

          :low_slide =>
          {
            :doc => generic_slide_doc(:low),
            :validations => [v_positive(:low_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :low_note =>
          {
            :doc => "Centre frequency of the low eq parameter in MIDI notes.",
            :validations => [v_greater_than(:low_note, 1)],
            :modulatable => true,
            :midi => true
          },

          :low_note_slide =>
          {
            :doc => generic_slide_doc(:low_note),
            :validations => [v_positive(:low_note_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :low_q =>
          {
            :doc => "The Q factor for the low eq parameter.

The Q factor controls the width of frequencies that will be affected by the low parameter of this eq FX. A low Q factor gives a wide bandwidth affecting a larger range of frequencies. A high Q factor will give a narrow bandwidth affecting a much smaller range of frequencies.

Here's a list of various Q factors and an approximate corresponding frequency width:

0.7     -> 2 octaves
1       -> 1 1/3 octaves
1.4     -> 1 octave
2.8     -> 1/2 octave
4.3     -> 1/3 octave
8.6     -> 1/6 octave

A decent range of Q factors for naturally sounding boosts/cuts is 0.6 to 1.
",
            :validations => [v_greater_than_oet(:low_q, 0.001), v_less_than_oet(:low_q, 100)],
            :modulatable => true
          },

          :low_q_slide =>
          {
            :doc => generic_slide_doc(:low_q),
            :validations => [v_positive(:low_q_slide)],
            :modulatable => true,
            :bpm_scale => true
          },


          :mid =>
          {
            :doc => "Gain - boost or cut the centre frequency of the middle part of the sound. The mid shelf defines the characteristics of the bass of the eq FX. A value of 0 will neither boost or cut the bass frequencies. A value of 1 will boost by 15 dB and a value of -1 will cut/attenuate by -15 dB.",
            :modulatable => true
          },

          :mid_slide =>
          {
            :doc => generic_slide_doc(:mid),
            :validations => [v_positive(:mid_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :mid_note =>
          {
            :doc => "Centre frequency of the mid eq parameter in MIDI notes.",
            :validations => [v_greater_than(:mid_note, 1)],
            :modulatable => true,
            :midi => true
          },

          :mid_note_slide =>
          {
            :doc => generic_slide_doc(:mid_note),
            :validations => [v_positive(:mid_note_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :mid_q =>
          {
            :doc => "The Q factor for the mid eq parameter.

The Q factor controls the width of frequencies that will be affected by the mid parameter of this eq FX. A mid Q factor gives a wide bandwidth affecting a larger range of frequencies. A high Q factor will give a narrow bandwidth affecting a much smaller range of frequencies.

Here's a list of various Q factors and an approximate corresponding frequency width:

0.7     -> 2 octaves
1       -> 1 1/3 octaves
1.4     -> 1 octave
2.8     -> 1/2 octave
4.3     -> 1/3 octave
8.6     -> 1/6 octave

A decent range of Q factors for naturally sounding boosts/cuts is 0.6 to 1.
",
            :validations => [v_greater_than_oet(:mid_q, 0.001), v_less_than_oet(:mid_q, 100)],
            :modulatable => true
          },

          :mid_q_slide =>
          {
            :doc => generic_slide_doc(:mid_q),
            :validations => [v_positive(:mid_q_slide)],
            :modulatable => true,
            :bpm_scale => true
          },


                    :high =>
          {
            :doc => "Gain - boost or cut the centre frequency of the high part of the sound. The high shelf defines the characteristics of the treble of the eq FX. A value of 0 will neither boost or cut the treble frequencies. A value of 1 will boost by 15 dB and a value of -1 will cut/attenuate by -15 dB.",
            :modulatable => true
          },

          :high_slide =>
          {
            :doc => generic_slide_doc(:high),
            :validations => [v_positive(:high_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :high_note =>
          {
            :doc => "Centre frequency of the high eq parameter in MIDI notes.",
            :validations => [v_greater_than(:high_note, 1)],
            :modulatable => true,
            :midi => true
          },

          :high_note_slide =>
          {
            :doc => generic_slide_doc(:high_note),
            :validations => [v_positive(:high_note_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :high_q =>
          {
            :doc => "The Q factor for the high eq parameter.

The Q factor controls the width of frequencies that will be affected by the high parameter of this eq FX. A high Q factor gives a wide bandwidth affecting a larger range of frequencies. A high Q factor will give a narrow bandwidth affecting a much smaller range of frequencies.

Here's a list of various Q factors and an approximate corresponding frequency width:

0.7     -> 2 octaves
1       -> 1 1/3 octaves
1.4     -> 1 octave
2.8     -> 1/2 octave
4.3     -> 1/3 octave
8.6     -> 1/6 octave

A decent range of Q factors for naturally sounding boosts/cuts is 0.6 to 1.
",
            :validations => [v_greater_than_oet(:high_q, 0.001), v_less_than_oet(:high_q, 100)],
            :modulatable => true
          },

          :high_q_slide =>
          {
            :doc => generic_slide_doc(:high_q),
            :validations => [v_positive(:high_q_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :high_shelf =>
          {
            :doc => "Gain - boost or cut the centre frequency. The high shelf defines the characteristics of the highest part of the eq FX. A value of 0 will neither boost or cut the high_shelf frequencies. A value of 1 will boost by 15 dB and a value of -1 will cut/attenuate by -15 dB.",
            :modulatable => true
          },

          :high_shelf_slide =>
          {
            :doc => generic_slide_doc(:high_shelf),
            :validations => [v_positive(:high_shelf_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :high_shelf_note =>
          {
            :doc => "Centre frequency of high shelf in MIDI notes.",
            :validations => [v_greater_than(:high_shelf_note, 1)],
            :modulatable => true,
            :midi => true
          },

          :high_shelf_note_slide =>
          {
            :doc => generic_slide_doc(:high_shelf_note),
            :validations => [v_positive(:high_shelf_note_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :high_shelf_slope =>
          {
            :doc => "High shelf boost/cut slope. When set to 1 (the default), the shelf slope is as steep as it can be and remain monotonically increasing or decreasing gain with frequency.",
            :validations => [v_greater_than_oet(:high_shelf_slope, 0), v_less_than_oet(:high_shelf_slope, 1)],
            :modulatable => true
          },

          :high_shelf_slope_slide =>
          {
            :doc => generic_slide_doc(:high_shelf_slope),
            :validations => [v_positive(:high_shelf_slope_slide)],
            :modulatable => true,
            :bpm_scale => true
          }

        }
      end
    end

    # class FXMcVerb < FXInfo
    #   def name
    #     "McVerb"
    #   end

    #   def introduced
    #     Version.new(2,9,0)
    #   end

    #   def synth_name
    #     "fx_mcverb"
    #   end

    #   def trigger_with_logical_clock?
    #     false
    #   end

    #   def doc
    #     "Reverb typically makes the incoming signal sound more spacious or distant as if it were played in a large room or cave. Similar to reverb but with a more spacious feel. This is a reverb based on an early design by SuperCollider creator James McCarthy which employs multiple allpass and comb filters to produce the effect."
    #   end

    #   def arg_defaults
    #     super.merge({
    #       :num_allpasses => 4,
    #       :num_combs => 7,
    #       :comb_rand => 0.3,
    #       :comb_rand_slide => 0,
    #       :comb_rand_slide_shape => 1,
    #       :comb_rand_slide_curve => 0,
    #       :allpass_rand => 0.15,
    #       :allpass_rand_slide => 0,
    #       :allpass_rand_slide_shape => 1,
    #       :allpass_rand_slide_curve => 0,
    #       :min_delay => 0.01,
    #       :min_delay_slide => 0,
    #       :min_delay_slide_shape => 1,
    #       :min_delay_slide_curve => 0,
    #       :max_delay => 0.09,
    #       :max_delay_slide => 0,
    #       :max_delay_slide_shape => 1,
    #       :max_delay_slide_curve => 0,
    #       :decay => 15,
    #       :pre_delay => 0.048,
    #       :seed => 0,
    #       :slope => 8

    #     })
    #   end

    #   def kill_delay(args_h)
    #     args_h[:decay] || arg_defaults[:decay]
    #   end

    #   def specific_arg_info
    #     {
    #       :num_allpasses =>
    #       {
    #         :doc => "Number of (chained) allpass filters to use.",
    #         :validations => [v_one_of(:num_allpasses, [1, 2, 3, 4])],
    #         :modulatable => true
    #       },

    #       :num_combs =>
    #       {
    #         :doc => "Number of comb filters to mix together ",
    #         :validations => [v_one_of(:num_combs, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])],
    #         :modulatable => true
    #       },

    #       :comb_rand =>
    #       {
    #         :doc => "Amount of (deterministic) randomisation to inject into comb filters. See the seed: opt to vary random choices.",
    #         :validations => [v_between_inclusive(:comb_rand, 0, 1)],
    #         :modulatable => true
    #       },

    #       :allpass_rand =>
    #       {
    #         :doc => "Amount of (deterministic) randomisation to inject into allpass filters. See the seed: opt to vary random choices.",
    #         :validations => [v_between_inclusive(:allpass_rand, 0, 1)],
    #         :modulatable => true
    #       },

    #       :min_delay =>
    #       {
    #         :doc => "High frequency rolloff of input signal. 0 is no damping (the reverb will ring out more) and 1 dampens the reverb signal completely",
    #         :validations => [v_between_inclusive(:pre_damp, 0, 1)],
    #         :modulatable => true
    #       },

    #     }
    #   end


    # end


    class FXGVerb < FXInfo

      def name
        "GVerb"
      end

      def introduced
        Version.new(2,9,0)
      end

      def synth_name
        "fx_gverb"
      end

      def trigger_with_logical_clock?
        false
      end

      def doc
        "Make the incoming signal sound more spacious or distant as if it were played in a large room or cave. Similar to reverb but with a more spacious feel."
      end


      def arg_defaults
        super.merge({
          :spread => 0.5,
          :spread_slide => 0,
          :spread_slide_shape => 1,
          :spread_slide_curve => 0,
          :damp => 0.5,
          :damp_slide => 0,
          :damp_slide_shape => 1,
          :damp_slide_curve => 0,
          :pre_damp => 0.5,
          :pre_damp_slide => 0,
          :pre_damp_slide_shape => 1,
          :pre_damp_slide_curve => 0,
          :dry => 1,
          :dry_slide => 0,
          :dry_slide_shape => 1,
          :dry_slide_curve => 0,
          :room => 10,
          :release => 3,
          :ref_level => 0.7,
          :tail_level => 0.5
        })
      end

      def kill_delay(args_h)
        args_h[:release] || arg_defaults[:release]
      end

      def specific_arg_info
        {
          :release =>
          {
            :doc => "Time for reverberation to complete in seconds",
            :validations => [v_greater_than(:release, 0)],
            :modulatable => true
          },

          :spread =>
          {
            :doc => "Stereo spread. Amount of stereo spread the reverb has over the left and right channels. A value of 0 means no spread at all - left and right stereo values of the incoming signal are preserved. A value of 1 means full spread - the left and right channels are fully mixed within the reverb - bleeding into each other.",
            :validations => [v_between_inclusive(:spread, 0, 1)],
            :modulatable => true
          },

          :damp =>
          {
            :doc => "High frequency rolloff. 0 is no damping (the reverb will ring out more) and 1 dampens the reverb signal completely",
            :validations => [v_between_inclusive(:damp, 0, 1)],
            :modulatable => true
          },

          :pre_damp =>
          {
            :doc => "High frequency rolloff of input signal. 0 is no damping (the reverb will ring out more) and 1 dampens the reverb signal completely",
            :validations => [v_between_inclusive(:pre_damp, 0, 1)],
            :modulatable => true
          },

          :dry =>
          {
            :doc => "Amount of original dry signal present in the effect. This is distinct from mix.",
            :validations => [v_greater_than_oet(:dry, 0)],
            :modulatable => true
          },

          :room =>
          {
            :doc => "The room size in squared metres",
            :validations => [v_greater_than_oet(:room, 1)],
            :modulatable => true
          },

          :ref_level =>
          {
            :doc => "Reflection level",
            :validations => [v_greater_than_oet(:ref_level, 0)],
            :modulatable => true
          },

          :tail_level =>
          {
            :doc => "Tail level amount",
            :validations => [v_greater_than_oet(:tail_level, 0)],
            :modulatable => true
          },

          :max_room =>
          {
            :doc => "Maximum room size",
            :validations => [v_greater_than(:max_room, 0)],
            :modulatable => false
          }
        }
      end
    end

    class FXReverb < FXInfo
      def name
        "Reverb"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "fx_reverb"
      end

      def trigger_with_logical_clock?
        false
      end

      def doc
        "Make the incoming signal sound more spacious or distant as if it were played in a large room or cave. Signal may also be dampened by reducing the amplitude of the higher frequencies."
      end

      def arg_defaults
        super.merge({
          :mix => 0.4,
          :room => 0.6,
          :room_slide => 0,
          :room_slide_shape => 1,
          :room_slide_curve => 0,
          :damp => 0.5,
          :damp_slide => 0,
          :damp_slide_shape => 1,
          :damp_slide_curve => 0,
        })
      end


      def kill_delay(args_h)
        r = args_h[:room] || arg_defaults[:room]
        [(r * 10) + 1, 11].min
      end


      def specific_arg_info
        {
          :room =>
          {
            :doc => "The room size - a value between 0 (no reverb) and 1 (maximum reverb).",
            :validations => [v_between_inclusive(:room, 0, 1)],
            :modulatable => true
          },

          :damp =>
          {
            :doc => "High frequency dampening - a value between 0 (no dampening) and 1 (maximum dampening)",
            :validations => [v_between_inclusive(:damp, 0, 1)],
            :modulatable => true
          },

          :room_slide =>
          {
            :doc => generic_slide_doc(:room),
            :validations => [v_positive(:room_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :damp_slide =>
          {
            :doc => generic_slide_doc(:damp),
            :validations => [v_positive(:damp_slide)],
            :modulatable => true,
            :bpm_scale => true
          }
        }
      end


    end

    class FXKrush < FXInfo
      def name
        "krush"
      end

      def introduced
        Version.new(2,6,0)
      end

      def synth_name
        "fx_krush"
      end

      def doc
        "Krush that sound!"
      end

      def arg_defaults
        super.merge({
          :gain => 5,
          :gain_slide => 0,
          :gain_slide_shape => 1,
          :gain_slide__curve => 0,
          :cutoff => 100,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,
          :res => 0,
          :res_slide => 0,
          :res_slide_shape => 1,
          :res_slide_curve => 0
        })
      end

      def specific_arg_info
        {
          :gain =>
          {
            :doc => "Amount of crushing to serve",
            :validations => [v_positive_not_zero(:gain)],
            :modulatable => true
          },

          :gain_slide =>
          {
            :doc => generic_slide_doc(:gain),
            :validations => [v_positive(:gain_slide)],
            :modulatable => true,
            :bpm_scale => true
          }
        }
      end

    end

    class FXBitcrusher < FXInfo
      def name
        "Bitcrusher"
      end

      def introduced
        Version.new(2,3,0)
      end

      def synth_name
        "fx_bitcrusher"
      end

      def doc
        "Creates lo-fi output by decimating and deconstructing the incoming audio by lowering both the sample rate and bit depth. The default sample rate for CD audio is 44100, so use values less than that for that crunchy chip-tune sound full of artefacts and bitty distortion. Similarly, the default bit depth for CD audio is 16, so use values less than that for lo-fi sound."
      end

      def arg_defaults
        super.merge({
          :sample_rate => 10000,
          :sample_rate_slide => 0,
          :sample_rate_slide_shape => 1,
          :sample_rate_slide_curve => 0,
          :bits => 8,
          :bits_slide => 0,
          :bits_slide_shape => 1,
          :bits_slide_curve => 0,
          :cutoff => 0,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0
        })
      end

      def specific_arg_info
        {
          :sample_rate =>
          {
            :doc => "The sample rate the audio will be resampled at. This represents the number of times per second the audio is sampled. The higher the sample rate, the closer to the original the sound will be, the lower the more low-fi it will sound. The highest sample rate is 44100 (full quality) and the lowest is ~100 (extremely low quality). Try values in between such as 1000, 3000, 8000...",
            :validations => [v_positive_not_zero(:sample_rate)],
            :modulatable => true
          },

          :bits =>
          {
            :doc => "The bit depth of the resampled audio. Lower bit depths make the audio sound grainy and less defined. The highest bit depth is 16 (full quality) and the lowest is 1 (lowest quality).",
            :validations => [v_positive_not_zero(:bits)],
            :modulatable => true
          },

          :sample_rate_slide =>
          {
            :doc => generic_slide_doc(:sample_rate),
            :validations => [v_positive(:sample_rate_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :bits_slide =>
          {
            :doc => generic_slide_doc(:bits),
            :validations => [v_positive(:bits_slide)],
            :modulatable => true,
            :bpm_scale => true
          }
        }
      end

    end

    class FXLevel < FXInfo
      def name
        "Level Amplifier"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "fx_level"
      end

      def doc
        "Amplitude modifier. All FX have their own amp built in, so it may be the case that you don't specifically need an isolated amp FX. However, it is useful to be able to control the overall amplitude of a number of running synths. All sounds created in the FX block will have their amplitudes multipled by the amp level of this FX. For example, use an amp of 0 to silence all internal synths."
      end

      def arg_defaults
        {
          :amp => 1,
          :amp_slide => 0,
          :amp_slide_shape => 1,
          :amp_slide_curve => 0,
        }
      end
    end

    class FXAutotuner < FXInfo
      def name
        "Autotuner"
      end

      def introduced
        Version.new(3,2,0)
      end

      def synth_name
        "fx_autotuner"
      end

      def doc
        "Autotune/phase vocoder effect. Used without any arguments, it tries to detect the pitch and shift it to the nearest exact note. This can help with out of tune singing, but it's also an interesting effect in its own right. When used with the note: arg, it tries to shift the input to match that note instead. This gives that classic \"robot singing\" sound that people associate with vocoders. This can then be changed using the control method to create new melodies.

```
with_fx :autotuner do |c|
```

```
  sample \"~/Downloads/acappella.wav\" # any sample with a voice is good
```

```
  sleep 4
```

```
  # listen to standard auto-tune behaviour for 4 seconds
```

```
  64.times do
```

```
     # now start changing note: to get robot voice behaviour
```

```
     control c, note: (scale :a2, :minor_pentatonic, num_octaves: 2).choose
```

```
     sleep 0.5
```

```
  end
```

```
end
```
"
      end

      def arg_defaults
        super.merge({
          :note => 0,
          :note_slide => 0,
          :note_slide_shape => 1,
          :note_slide_curve => 0,
          :formant_ratio => 1.0,
          :formant_ratio_slide => 0,
          :formant_ratio_slide_shape => 1,
          :formant_ratio_slide_curve => 0
          #TODO: Add documentation:
          # comment out these until documentation is added
          # :transpose => 0,
          # :min_freq => 80,
          # :max_formant_ratio => 10,
          # :grains_period => 2.0,
        })
      end

      def specific_arg_info
        {
          :note =>
          {
            :doc => "Midi note to shift pitch to. The quality of the sound depends on how stable the pitch of the input is.",
            :validations => [v_between_inclusive(:note, 0, 127)],
            :modulatable => true,
            :midi => true
          },

          :formant_ratio =>
          {
            :doc => "This effect separates pitched content of an input from the formant sounds (percussive, non-pitched sounds like \"ssss\" and \"ttttt\"). Changing the formant ratio shifts the non-pitched sounds - lower pitched formants (0.5) sound like someone with a deep voice, higher values (e.g. 2.0 and above) sound like a high pitched voice.",
            :validations => [v_between_inclusive(:formant_ratio, 0, 10)],
            :modulatable => true
          }
        }
      end
    end

    class FXMono < FXInfo
      def name
        "Mono"
      end

      def introduced
        Version.new(2,10,0)
      end

      def synth_name
        "fx_mono"
      end

      def doc
        "Sum left and right channels. Useful with stereo samples that you need as a mono sound, or for use with panslicer."
      end

      def arg_defaults
        super.merge({
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0
        })
      end
    end

    class FXEcho < FXInfo
      def name
        "Echo"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "fx_echo"
      end

      def doc
        "Standard echo with variable phase duration (time between echoes) and decay (length of echo fade out). If you wish to have a phase duration longer than 2s, you need to specify the longest phase duration you'd like with the arg max_phase. Be warned, echo FX with very long phases can consume a lot of memory and take longer to initialise."
      end

      def arg_defaults
        super.merge({
          :phase => 0.25,
          :phase_slide => 0,
          :phase_slide_shape => 1,
          :phase_slide_curve => 0,
          :decay => 2,
          :decay_slide => 0,
          :decay_slide_shape => 1,
          :decay_slide_curve => 0,
          :max_phase => 2
        })
      end

      def specific_arg_info
        {
          :max_phase =>
          {
            :doc => "The maximum phase duration in beats.",
            :validations => [v_positive_not_zero(:max_phase)],
            :modulatable => false,
            :bpm_scale => true
          },

          :phase =>
          {
            :doc => "The time between echoes in beats.",
            :validations => [v_positive_not_zero(:phase)],
            :modulatable => true,
            :bpm_scale => true

          },

          :phase_slide =>
          {
            :doc => "Slide time in beats between phase values",
            :validations => [v_positive(:phase_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :decay =>
          {
            :doc => "The time it takes for the echoes to fade away in beats.",
            :validations => [v_positive_not_zero(:decay)],
            :modulatable => true,
            :bpm_scale => true
          },

          :decay_slide =>
          {
            :doc => "Slide time in beats between decay times",
            :validations => [v_positive(:decay_slide)],
            :modulatable => true,
            :bpm_scale => true
          }
        }
      end

      def kill_delay(args_h)
        args_h[:decay] || arg_defaults[:decay]
      end

    end

    class FXSlicer < FXInfo
      def name
        "Slicer"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "fx_slicer"
      end

      def on_start(studio, args_h)
        args_h[:rand_buf] = studio.rand_buf_id
      end

      def doc
        "Modulates the amplitude of the input signal with a specific control wave and phase duration. With the default pulse wave, slices the signal in and out, with the triangle wave, fades the signal in and out and with the saw wave, phases the signal in and then dramatically out. Control wave may be inverted with the arg invert_wave for more variety."
      end

      def arg_defaults
        super.merge({
          :phase => 0.25,
          :phase_slide => 0,
          :phase_slide_shape => 1,
          :phase_slide_curve => 0,
          :amp_min => 0,
          :amp_min_slide => 0,
          :amp_min_slide_shape => 1,
          :amp_min_slide_curve => 0,
          :amp_max => 1,
          :amp_max_slide => 0,
          :amp_max_slide_shape => 1,
          :amp_max_slide_curve => 0,
          :pulse_width => 0.5,
          :pulse_width_slide => 0,
          :pulse_width_slide_shape => 1,
          :pulse_width_slide_curve => 0,
          :phase_offset => 0,
          :wave => 1,
          :invert_wave => 0,
          :probability => 0,
          :probability_slide => 0,
          :probability_slide_shape => 1,
          :probability_slide_curve => 0,
          :prob_pos => 0,
          :prob_pos_slide => 0,
          :prob_pos_slide_shape => 1,
          :prob_pos_slide_curve => 0,
          :seed => 0,
          :smooth => 0,
          :smooth_slide => 0,
          :smooth_slide_shape => 1,
          :smooth_slide_curve => 0,
          :smooth_up => 0,
          :smooth_up_slide => 0,
          :smooth_up_slide_shape => 1,
          :smooth_up_slide_curve => 0,
          :smooth_down => 0,
          :smooth_down_slide => 0,
          :smooth_down_slide_shape => 1,
          :smooth_down_slide_curve => 0
        })
      end

      def specific_arg_info
        {
          :smooth =>
          {
            :doc => "Amount of time in seconds to transition from the current value to the next. Allows you to round off harsh edges in the slicer wave which may cause clicks.",
            :validations => [v_positive(:smooth)],
            :modulatable => true
          },

          :smooth_slide =>
          {
            :doc => generic_slide_doc(:smooth),
            :validations => [v_positive(:smooth_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :smooth_up =>
          {
            :doc => "Amount of time in seconds to transition from the current value to the next only when the value is going up. This smoothing happens before the main smooth mechanism.",
            :validations => [v_positive(:smooth_up)],
            :modulatable => true
          },

          :smooth_up_slide =>
          {
            :doc => generic_slide_doc(:smooth_up),
            :validations => [v_positive(:smooth_up_slide)],
            :modulatable => true,
            :bpm_scale => true
          },


          :smooth_down =>
          {
            :doc => "Amount of time in seconds to transition from the current value to the next only when the value is going down. This smoothing happens before the main smooth mechanism.",
            :validations => [v_positive(:smooth_down)],
            :modulatable => true
          },

          :smooth_down_slide =>
          {
            :doc => generic_slide_doc(:smooth_down),
            :validations => [v_positive(:smooth_down_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :probability =>
          {
            :doc => "Probability (as a value between 0 and 1) that a given slice will be replaced by the value of the  prob_pos opt (which defaults to 0, i.e. silence)",
            :validations => [v_between_inclusive(:probability, 0, 1)],
            :modulatable => true
          },

          :probability_slide =>
          {
            :doc => generic_slide_doc(:probability),
            :validations => [v_positive(:probability_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :prob_pos =>
          {
            :doc => "Position of the slicer that will be jumped to when the probability test passes as a value between 0 and 1",
            :validations => [v_between_inclusive(:prob_pos, 0, 1)],
            :modulatable => true
          },

          :prob_pos_slide =>
          {
            :doc => generic_slide_doc(:prob_pos),
            :validations => [v_positive(:prob_pos_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :seed =>
          {
            :doc => "Seed value for rand num generator used for probability test",
            :modulatable => false
          },


          :phase =>
          {
            :doc => "The phase duration (in beats) of the slices",
            :validations => [v_positive_not_zero(:phase)],
            :modulatable => true,
            :bpm_scale => true
          },

          :phase_slide =>
          {
            :doc => "Slide time in beats between phase values",
            :validations => [v_positive(:phase_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :width =>
          {
            :doc => "The width of the slices - 0 - 1.",
            :validations => [v_between_exclusive(:width, 0, 1)],
            :modulatable => true
          },

          :width_slide =>
          {
            :doc => "Slide time in beats between width values",
            :validations => [v_positive(:width_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :phase_offset=>
          {
            :doc => "Initial phase offset.",
            :validations => [v_between_inclusive(:phase_offset, 0, 1)],
            :modulatable => false
          },

          :amp_slide =>
          {
            :doc => "The slide lag time for amplitude changes.",
            :validations => [v_positive(:amp_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :amp =>
          {
            :doc => "The amplitude of the resulting effect.",
            :validations => [v_positive(:amp)],
            :modulatable => true
          },

          :wave =>
          {
            :doc => "Control waveform used to modulate the amplitude. 0=saw, 1=pulse, 2=tri, 3=sine",
            :validations => [v_one_of(:wave, [0, 1, 2, 3])],
            :modulatable => true
          },

          :invert_wave =>
          {
            :doc => "Invert control waveform (i.e. flip it on the y axis). 0=uninverted wave, 1=inverted wave.",
            :validations => [v_one_of(:invert_wave, [0, 1])],
            :modulatable => true
          },

          :amp_min =>
          {
            :doc => "Minimum amplitude of the slicer",
            :validations => [v_positive(:amp_min)],
            :modulatable => true
          },

          :amp_min_slide =>
          {
            :doc => generic_slide_doc(:amp_min),
            :validations => [v_positive(:amp_min_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :amp_max =>
          {
            :doc => "Maximum amplitude of the slicer",
            :validations => [v_positive(:amp_max)],
            :modulatable => true
          },

          :amp_max_slide =>
          {
            :doc => generic_slide_doc(:amp_max),
            :validations => [v_positive(:amp_max_slide)],
            :modulatable => true,
            :bpm_scale => true
          }

        }
      end
    end

    class FXWobble < FXInfo
      def name
        "Wobble"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "fx_wobble"
      end

      def on_start(studio, args_h)
        args_h[:rand_buf] = studio.rand_buf_id
      end

      def doc
        "Versatile wobble FX. Will repeatedly modulate a range of filters (rlpf, rhpf) between two cutoff values using a range of control wave forms (saw, pulse, tri, sine). You may alter the phase duration of the wobble, and the resonance of the filter. Combines well with the dsaw synth for crazy dub wobbles. Cutoff value is at cutoff_min at the start of phase"
      end

      def arg_defaults
        super.merge({
          :phase => 0.5,
          :phase_slide => 0,
          :phase_slide_shape => 1,
          :phase_slide_curve => 0,
          :cutoff_min => 60,
          :cutoff_min_slide => 0,
          :cutoff_min_slide_shape => 1,
          :cutoff_min_slide_curve => 0,
          :cutoff_max => 120,
          :cutoff_max_slide => 0,
          :cutoff_max_slide_shape => 1,
          :cutoff_max_slide_curve => 0,
          :res => 0.8,
          :res_slide => 0,
          :res_slide_shape => 1,
          :res_slide_curve => 0,
          :phase_offset => 0,
          :wave => 0,
          :invert_wave => 0,
          :pulse_width => 0.5,
          :pulse_width_slide => 0,
          :pulse_width_slide_shape => 1,
          :pulse_width_slide_curve => 0,
          :filter => 0,
          :probability => 0,
          :probability_slide => 0,
          :probability_slide_shape => 1,
          :probability_slide_curve => 0,
          :prob_pos => 0,
          :prob_pos_slide => 0,
          :prob_pos_slide_shape => 1,
          :prob_pos_slide_curve => 0,
          :seed => 0,
          :smooth => 0,
          :smooth_slide => 0,
          :smooth_slide_shape => 1,
          :smooth_slide_curve => 0,
          :smooth_up => 0,
          :smooth_up_slide => 0,
          :smooth_up_slide_shape => 1,
          :smooth_up_slide_curve => 0,
          :smooth_down => 0,
          :smooth_down_slide => 0,
          :smooth_down_slide_shape => 1,
          :smooth_down_slide_curve => 0
        })
      end

      def specific_arg_info
        {
          :invert_wave =>
          {
            :doc => "Invert control waveform (i.e. flip it on the y axis). 0=uninverted wave, 1=inverted wave.",
            :validations => [v_one_of(:invert_wave, [0, 1])],
            :modulatable => true
          },

          :smooth =>
          {
            :doc => "Amount of time in seconds to transition from the current value to the next. Allows you to round off harsh edges in the slicer wave which may cause clicks.",
            :validations => [v_positive(:smooth)],
            :modulatable => true
          },

          :smooth_slide =>
          {
            :doc => generic_slide_doc(:smooth),
            :validations => [v_positive(:smooth_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :smooth_up =>
          {
            :doc => "Amount of time in seconds to transition from the current value to the next only when the value is going up. This smoothing happens before the main smooth mechanism.",
            :validations => [v_positive(:smooth_up)],
            :modulatable => true
          },

          :smooth_up_slide =>
          {
            :doc => generic_slide_doc(:smooth_up),
            :validations => [v_positive(:smooth_up_slide)],
            :modulatable => true,
            :bpm_scale => true
          },


          :smooth_down =>
          {
            :doc => "Amount of time in seconds to transition from the current value to the next only when the value is going down. This smoothing happens before the main smooth mechanism.",
            :validations => [v_positive(:smooth_down)],
            :modulatable => true
          },

          :smooth_down_slide =>
          {
            :doc => generic_slide_doc(:smooth_down),
            :validations => [v_positive(:smooth_down_slide)],
            :modulatable => true,
            :bpm_scale => true
          },



          :probability =>
          {
            :doc => "Probability (as a value between 0 and 1) that a given wobble will be replaced by the value of the  prob_pos opt (which defaults to 0, i.e. min_cutoff)",
            :validations => [v_between_inclusive(:probability, 0, 1)],
            :modulatable => true
          },

          :probability_slide =>
          {
            :doc => generic_slide_doc(:probability),
            :validations => [v_positive(:probability_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :prob_pos =>
          {
            :doc => "Position of the wobble that will be jumped to when the probability test passes as a value between 0 and 1",
            :validations => [v_between_inclusive(:prob_pos, 0, 1)],
            :modulatable => true
          },

          :prob_pos_slide =>
          {
            :doc => generic_slide_doc(:prob_pos),
            :validations => [v_positive(:prob_pos_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :seed =>
          {
            :doc => "Seed value for rand num generator used for probability test",
            :modulatable => false
          },


          :cutoff_min =>
          {
            :doc => "Minimum (MIDI) note that filter will move to whilst wobbling. Choose a lower note for a higher range of movement. Full range of movement is the distance between cutoff_max and cutoff_min",
            :validations => [v_positive(:cutoff_min), v_less_than(:cutoff_min, 130)],
            :modulatable => true,
            :midi => true
          },

          :cutoff_min_slide =>
          {
            :doc => generic_slide_doc(:cutoff_min),
            :validations => [v_positive(:cutoff_min_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :cutoff_max =>
          {
            :doc => "Maximum (MIDI) note that filter will move to whilst wobbling. Choose a higher note for a higher range of movement. Full range of movement is the distance between cutoff_max and cutoff_min",
            :validations => [v_positive(:cutoff_max), v_less_than(:cutoff_max, 130)],
            :modulatable => true,
            :midi => true
          },

          :cutoff_max_slide =>
          {
            :doc => generic_slide_doc(:cutoff_max),
            :validations => [v_positive(:cutoff_max_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :phase =>
          {
            :doc => "The phase duration (in beats) for filter modulation cycles",
            :validations => [v_positive_not_zero(:phase)],
            :modulatable => true,
            :bpm_scale => true
          },

          :phase_slide =>
          {
            :doc => generic_slide_doc(:phase),
            :validations => [v_positive(:phase_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :pulse_width =>
          {
            :doc => "Only valid if wave is type pulse.",
            :validations => [v_positive(:pulse_width)],
            :modulatable => true
          },

          :pulse_width_slide =>
          {
            :doc => "Time in beats for pulse width to change. Only valid if wave is type pulse.",
            :validations => [v_positive(:pulse_width_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :wave =>
          {
            :doc => "Wave shape of wobble. Use 0 for saw wave, 1 for pulse, 2 for triangle wave and 3 for a sine wave.",
            :validations => [v_one_of(:wave, [0, 1, 2, 3])],
            :modulatable => true
          },

          :filter =>
          {
            :doc => "Filter used for wobble effect. Use 0 for a resonant low pass filter or 1 for a resonant high pass filter",
            :validations => [v_one_of(:filter, [0, 1])],
            :modulatable => true
          }
        }
      end
    end

    class FXPanSlicer < FXSlicer
      def name
        "Pan Slicer"
      end

      def introduced
        Version.new(2,6,0)
      end

      def synth_name
        "fx_panslicer"
      end

      def on_start(studio, args_h)
        args_h[:rand_buf] = studio.rand_buf_id
      end

      def doc
        "Slice the pan automatically from left to right. Behaves similarly to slicer and wobble FX but modifies stereo panning of sound in left and right speakers. Default slice wave form is square (hard slicing between left and right) however other wave forms can be set with the `wave:` opt."
      end

      def arg_defaults
        super.merge({
          :phase => 0.25,
          :phase_slide => 0,
          :phase_slide_shape => 1,
          :phase_slide_curve => 0,
          :pan_min => -1,
          :pan_min_slide => 0,
          :pan_min_slide_shape => 1,
          :pan_min_slide_curve => 0,
          :pan_max => 1,
          :pan_max_slide => 0,
          :pan_max_slide_shape => 1,
          :pan_max_slide_curve => 0,
          :pulse_width => 0.5,
          :pulse_width_slide => 0,
          :pulse_width_slide_shape => 1,
          :pulse_width_slide_curve => 0,
          :phase_offset => 0,
          :wave => 1,
          :invert_wave => 0,
          :probability => 0,
          :probability_slide => 0,
          :probability_slide_shape => 1,
          :probability_slide_curve => 0,
          :prob_pos => 0,
          :prob_pos_slide => 0,
          :prob_pos_slide_shape => 1,
          :prob_pos_slide_curve => 0,
          :seed => 0,
          :smooth => 0,
          :smooth_slide => 0,
          :smooth_slide_shape => 1,
          :smooth_slide_curve => 0,
          :smooth_up => 0,
          :smooth_up_slide => 0,
          :smooth_up_slide_shape => 1,
          :smooth_up_slide_curve => 0,
          :smooth_down => 0,
          :smooth_down_slide => 0,
          :smooth_down_slide_shape => 1,
          :smooth_down_slide_curve => 0
        })
      end

      def specific_arg_info
        {
          :smooth =>
          {
            :doc => "Amount of time in seconds to transition from the current value to the next. Allows you to round off harsh edges in the slicer wave which may cause clicks.",
            :validations => [v_positive(:smooth)],
            :modulatable => true
          },

          :smooth_slide =>
          {
            :doc => generic_slide_doc(:smooth),
            :validations => [v_positive(:smooth_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :smooth_up =>
          {
            :doc => "Amount of time in seconds to transition from the current value to the next only when the value is going up. This smoothing happens before the main smooth mechanism.",
            :validations => [v_positive(:smooth_up)],
            :modulatable => true
          },

          :smooth_up_slide =>
          {
            :doc => generic_slide_doc(:smooth_up),
            :validations => [v_positive(:smooth_up_slide)],
            :modulatable => true,
            :bpm_scale => true
          },


          :smooth_down =>
          {
            :doc => "Amount of time in seconds to transition from the current value to the next only when the value is going down. This smoothing happens before the main smooth mechanism.",
            :validations => [v_positive(:smooth_down)],
            :modulatable => true
          },

          :smooth_down_slide =>
          {
            :doc => generic_slide_doc(:smooth_down),
            :validations => [v_positive(:smooth_down_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :probability =>
          {
            :doc => "Probability (as a value between 0 and 1) that a given slice will be replaced by the value of the  prob_pos opt (which defaults to 0, i.e. silence)",
            :validations => [v_between_inclusive(:probability, 0, 1)],
            :modulatable => true
          },

          :probability_slide =>
          {
            :doc => generic_slide_doc(:probability),
            :validations => [v_positive(:probability_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :prob_pos =>
          {
            :doc => "Position of the slicer that will be jumped to when the probability test passes as a value between 0 and 1",
            :validations => [v_between_inclusive(:prob_pos, 0, 1)],
            :modulatable => true
          },

          :prob_pos_slide =>
          {
            :doc => generic_slide_doc(:prob_pos),
            :validations => [v_positive(:prob_pos_slide)],
            :modulatable => true,
            :bpm_scale => true
          },


          :seed =>
          {
            :doc => "Seed value for rand num generator used for probability test",
            :modulatable => false
          },


          :phase =>
          {
            :doc => "The phase duration (in beats) of the slices",
            :validations => [v_positive_not_zero(:phase)],
            :modulatable => true,
            :bpm_scale => true
          },

          :phase_slide =>
          {
            :doc => "Slide time in beats between phase values",
            :validations => [v_positive(:phase_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :width =>
          {
            :doc => "The width of the slices - 0 - 1.",
            :validations => [v_between_exclusive(:width, 0, 1)],
            :modulatable => true
          },

          :width_slide =>
          {
            :doc => "Slide time in beats between width values",
            :validations => [v_positive(:width_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :phase_offset=>
          {
            :doc => "Initial phase offset.",
            :validations => [v_between_inclusive(:phase_offset, 0, 1)],
            :modulatable => false
          },

          :amp_slide =>
          {
            :doc => "The slide lag time for amplitude changes.",
            :validations => [v_positive(:amp_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :amp =>
          {
            :doc => "The amplitude of the resulting effect.",
            :validations => [v_positive(:amp)],
            :modulatable => true
          },

          :wave =>
          {
            :doc => "Control waveform used to modulate the amplitude. 0=saw, 1=pulse, 2=tri, 3=sine",
            :validations => [v_one_of(:wave, [0, 1, 2, 3])],
            :modulatable => true
          },

          :invert_wave =>
          {
            :doc => "Invert control waveform (i.e. flip it on the y axis). 0=uninverted wave, 1=inverted wave.",
            :validations => [v_one_of(:invert_wave, [0, 1])],
            :modulatable => true
          },

          :pan_min =>
          {
            :doc => "Minimum pan value (-1 is the left speaker only)",
            :validations => [v_between_inclusive(:pan_min, -1, 1)],
            :modulatable => true
          },

          :pan_min_slide =>
          {
            :doc => generic_slide_doc(:pan_min),
            :validations => [v_positive(:pan_min_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :pan_max =>
          {
            :doc => "Maximum pan value (+1 is the right speaker only)",
            :validations => [v_between_inclusive(:pan_max, -1, 1)],
            :modulatable => true
          },

          :pan_max_slide =>
          {
            :doc => generic_slide_doc(:pan_max),
            :validations => [v_positive(:pan_max_slide)],
            :modulatable => true,
            :bpm_scale => true
          }
        }

      end
    end

    class FXIXITechno < FXInfo
      def name
        "Techno from IXI Lang"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "fx_ixi_techno"
      end

      def doc
        "Moving resonant low pass filter between min and max cutoffs. Great for sweeping effects across long synths or samples."
      end

      def arg_defaults
        super.merge({
          :phase => 4,
          :phase_slide => 0,
          :phase_slide_shape => 1,
          :phase_slide_curve => 0,
          :phase_offset => 0,
          :cutoff_min => 60,
          :cutoff_min_slide => 0,
          :cutoff_min_slide_shape => 1,
          :cutoff_min_slide_curve => 0,
          :cutoff_max => 120,
          :cutoff_max_slide => 0,
          :cutoff_max_slide_shape => 1,
          :cutoff_max_slide_curve => 0,
          :res => 0.8,
          :res_slide => 0,
          :res_slide_shape => 1,
          :res_slide_curve => 0,
        })
      end

      def specific_arg_info
        {
          :phase =>
          {
            :doc => "The phase duration (in beats) for filter modulation cycles",
            :validations => [v_positive_not_zero(:phase)],
            :modulatable => true,
            :bpm_scale => true
          },

          :phase_slide =>
          {
            :doc => generic_slide_doc(:phase),
            :validations => [v_positive(:phase_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :phase_offset =>
          {
            :doc => "Initial modulation phase offset (a value between 0 and 1).",
            :validations => [v_between_inclusive(:phase_offset, 0, 1)],
            :modulatable => false
          },

          :cutoff_min =>
          {
            :doc => "Minimum (MIDI) note that filter will move to whilst wobbling. Choose a lower note for a higher range of movement. Full range of movement is the distance between cutoff_max and cutoff_min",
            :validations => [v_positive(:cutoff_min), v_less_than(:cutoff_min, 130)],
            :modulatable => true,
            :midi => true
          },

          :cutoff_min_slide =>
          {
            :doc => generic_slide_doc(:cutoff_min),
            :validations => [v_positive(:cutoff_min_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :cutoff_max =>
          {
            :doc => "Maximum (MIDI) note that filter will move to whilst wobbling. Choose a higher note for a higher range of movement. Full range of movement is the distance between cutoff_max and cutoff_min",
            :validations => [v_positive(:cutoff_max), v_less_than(:cutoff_max, 130)],
            :modulatable => true,
            :midi => true
          },

          :cutoff_max_slide =>
          {
            :doc => generic_slide_doc(:cutoff_max),
            :validations => [v_positive(:cutoff_max_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :filter =>
          {
            :doc => "Filter used for wobble effect. Use 0 for a resonant low pass filter or 1 for a resonant high pass filter",
            :validations => [v_one_of(:filter, [0, 1])],
            :modulatable => true
          }

        }
      end
    end

    class FXWhammy < FXInfo
      def name
        "Whammy"
      end

      def introduced
        Version.new(2,10,0)
      end

      def synth_name
        "fx_whammy"
      end

      def doc
        "A cheap sounding transposition effect, with a slightly robotic edge. Good for adding alien sounds and harmonies to everything from beeps to guitar samples. It's similar to pitch shift although not as smooth sounding."
      end

      def arg_defaults
        super.merge({
          :transpose => 12,
          :transpose_slide => 0,
          :transpose_slide_shape => 1,
          :transpose_slide_curve => 0,
          :max_delay_time => 1,
          :deltime => 0.05,
          :grainsize => 0.075
        })
      end

      def specific_arg_info
        {

          :transpose =>
          {
            :doc => "This is how much to transpose the input, expressed as a midi pitch.",
            :modulatable => true
          },

          :transpose_slide =>
          {
            :doc => generic_slide_doc(:transpose),
            :validations => [v_positive(:transpose_slide)],
            :modulatable => true,
          },

          :deltime =>
          {
            :doc => "The delay time to be used for the effect. This shouldn't need to be adjusted.",
            :validations => [v_positive(:deltime)],
          },

          :max_delay_time =>
          {
            :doc => "The max delay time to be used for the effect. This shouldn't need to be adjusted.",
            :validations => [v_positive(:max_delay_time)],
          },

          :grainsize =>
          {
            :doc => "The size of the initial grain used for transposition. This shouldn't need to be adjusted.",
            :validations => [v_positive(:grainsize)],
          },
        }
      end
    end

    class FXCompressor < FXInfo
      def name
        "Compressor"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "fx_compressor"
      end

      def doc
        "Compresses the dynamic range of the incoming signal. Equivalent to automatically turning the amp down when the signal gets too loud and then back up again when it's quiet. Useful for ensuring the containing signal doesn't overwhelm other aspects of the sound. Also a general purpose hard-knee dynamic range processor which can be tuned via the opts to both expand and compress the signal."
      end

      def arg_defaults
        super.merge({
          :threshold => 0.2,
          :threshold_slide => 0,
          :threshold_slide_shape => 1,
          :threshold_slide_curve => 0,
          :clamp_time => 0.01,
          :clamp_time_slide => 0,
          :clamp_time_slide_shape => 1,
          :clamp_time_slide_curve => 0,
          :slope_above => 0.5,
          :slope_above_slide => 0,
          :slope_above_slide_shape => 1,
          :slope_above_slide_curve => 0,
          :slope_below => 1,
          :slope_below_slide => 0,
          :slope_below_slide_shape => 1,
          :slope_below_slide_curve => 0,
          :relax_time => 0.01,
          :relax_time_slide => 0,
          :relax_time_slide_shape => 1,
          :relax_time_slide_curve => 0,
        })
      end

      def specific_arg_info
        {

          :threshold =>
          {
            :doc => "Threshold value determining the break point between slope_below and slope_above.",
            :validations => [v_positive(:threshold)],
            :modulatable => true
          },

          :threshold_slide =>
          {
            :doc => generic_slide_doc(:threshold),
            :validations => [v_positive(:threshold_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :slope_below =>
          {
            :doc => "Slope of the amplitude curve below the threshold. A value of 1 means that the output of signals with amplitude below the threshold will be unaffected. Greater values will attenuate and smaller values will magnify the signal.",
            :validations => [],
            :modulatable => true
          },

          :slope_below_slide =>
          {
            :doc => generic_slide_doc(:slope_below),
            :validations => [v_positive(:slope_below_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :slope_above =>
          {
            :doc => "Slope of the amplitude curve above the threshold. A value of 1 means that the output of signals with amplitude above the threshold will be unaffected. Greater values will magnify and smaller values will attenuate the signal.",

            :validations => [],
            :modulatable => true
          },

          :slope_above_slide =>
          {
            :doc => generic_slide_doc(:slope_above),
            :validations => [v_positive(:slope_above_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :clamp_time =>
          {
            :doc => "Time taken for the amplitude adjustments to kick in fully (in seconds). This is usually pretty small (not much more than 10 milliseconds). Also known as the time of the attack phase",
            :validations => [v_positive(:clamp_time)],
            :modulatable => true
          },

          :clamp_time_slide =>
          {
            :doc => generic_slide_doc(:clamp_time),
            :validations => [v_positive(:clamp_time_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :relax_time =>
          {
            :doc => "Time taken for the amplitude adjustments to be released. Usually a little longer than clamp_time. If both times are too short, you can get some (possibly unwanted) artefacts. Also known as the time of the release phase.",
            :validations => [v_positive(:relax_time)],
            :modulatable => true
          },

          :relax_time_slide =>
          {
            :doc => generic_slide_doc(:relax_time),
            :validations => [v_positive(:relax_time_slide)],
            :modulatable => true,
            :bpm_scale => true
          }
        }
      end
    end

    class FXVowel < FXInfo
      def name
        "Vowel"
      end

      def introduced
        Version.new(2,10,0)
      end

      def synth_name
        "fx_vowel"
      end

      def arg_defaults
        super.merge({
          :vowel_sound => 1,
          :voice => 0
        })
      end

      def specific_arg_info
        {
          :vowel_sound =>
          {
            :doc => "1,2,3,4,5 => A,E,I,O,U",
            :validations => [v_one_of(:vowel_sound, [1,2,3,4,5])],
            :modulatable => true
          },
          :voice =>
          {
            :doc => "0,1,2,3,4 => Soprano,Alto,Counter Tenor, Tenor, Bass",
            :validations => [v_one_of(:voice, [0,1,2,3,4])],
            :modulatable => true
          }
        }
      end

      def doc
        "This effect filters the input to match a human voice singing a certain vowel sound. Human singing voice sounds are easily achieved with a source of a saw wave with a little vibrato."
      end
    end

    class FXOctaver < FXInfo
      def name
        "Octaver"
      end

      def introduced
        Version.new(2,2,0)
      end

      def synth_name
        "fx_octaver"
      end

      def arg_defaults
        super.merge({
          :super_amp => 1,
          :super_amp_slide => 0,
          :super_amp_slide_shape => 1,
          :super_amp_slide_curve => 0,
          :sub_amp => 1,
          :sub_amp_slide => 0,
          :sub_amp_slide_shape => 1,
          :sub_amp_slide_curve => 0,
          :subsub_amp => 1,
          :subsub_amp_slide => 0,
          :subsub_amp_slide_shape => 1,
          :subsub_amp_slide_curve => 0
        })
      end

      def specific_arg_info
        {
          :super_amp =>
          {
            :doc => "Volume of the signal 1 octave above the input",
            :validations => [v_positive(:super_amp)],
            :modulatable => true
          },
          :sub_amp =>
          {
            :doc => "Volume of the signal 1 octave below the input",
            :validations => [v_positive(:sub_amp)],
            :modulatable => true
          },
          :subsub_amp =>
          {
            :doc => "Volume of the signal 2 octaves below the input",
            :validations => [v_positive(:subsub_amp)],
            :modulatable => true
          }
        }
      end

      def doc
        "This effect adds three pitches based on the input sound. The first is the original sound transposed up an octave (super_amp), the second is the original sound transposed down an octave (sub_amp) and the third is the original sound transposed down two octaves (subsub_amp).

  The way the transpositions are done adds some distortion/fuzz, particularly to the lower octaves, whilst the upper octave has a 'cheap' quality. This effect is often used in guitar effects pedals but it can work with other sounds too. There's a great description of the science behind this on Wikipedia here: https://en.wikipedia.org/wiki/Octave_effect"
      end
    end

    class FXChorus < FXInfo
      def name
        "Chorus"
      end

      def introduced
        Version.new(2,2,0)
      end

      def synth_name
        "fx_chorus"
      end

      def arg_defaults
        super.merge({
          :phase => 0.25,
          :phase_slide => 0,
          :phase_slide_shape => 1,
          :phase_slide_curve => 0,
          :decay => 0.00001,
          :decay_slide => 0,
          :decay_slide_shape => 1,
          :decay_slide_curve => 0,
          :max_phase => 1
        })
      end

      def specific_arg_info
        {
          :max_phase =>
          {
            :doc => "The maximum phase duration in beats.",
            :validations => [v_positive_not_zero(:max_phase)],
            :modulatable => false,
            :bpm_scale => true
          },

          :phase =>
          {
            :doc => "The time between echoes in beats (phase duration).",
            :validations => [v_positive_not_zero(:phase)],
            :modulatable => true,
            :bpm_scale => false
          },

          :phase_slide =>
          {
            :doc => "Slide time in beats between phase values",
            :validations => [v_positive(:phase_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :decay =>
          {
            :doc => "The time it takes for the echoes to fade away in beats.",
            :validations => [v_positive_not_zero(:decay)],
            :modulatable => true,
            :bpm_scale => true
          },

          :decay_slide =>
          {
            :doc => "Slide time in beats between decay times",
            :validations => [v_positive(:decay_slide)],
            :modulatable => true,
            :bpm_scale => true
          }
        }
      end

      def kill_delay(args_h)
        args_h[:decay] || arg_defaults[:decay]
      end

      def doc
        "Standard chorus with variable phase duration (time between echoes). A type of short echo that usually makes the sound \"thicker\". If you wish to have a phase duration longer than 2 beats, you need to specify the longest phase duration you'd like with the arg max_phase. Be warned, as with echo, chorus FX with very long phases can consume a lot of memory and take longer to initialise."
      end
    end

    class FXRingMod < FXInfo
      def name
        "Ring Modulator"
      end

      def introduced
        Version.new(2,3,0)
      end

      def synth_name
        "fx_ring_mod"
      end

      def arg_defaults
        super.merge({
          :freq => 30,
          :freq_slide => 0,
          :freq_slide_shape => 1,
          :freq_slide_curve => 0,
          :mod_amp => 1,
          :mod_amp_slide => 0,
          :mod_amp_slide_shape => 1,
          :mod_amp_slide_curve => 0,
        })
      end

      def specific_arg_info
        {
          :freq =>
          {
            :doc => "Frequency of the carrier signal (as a midi note).",
            :validations => [v_positive_not_zero(:freq)],
            :modulatable => true,
            :midi => true
          },

          :freq_slide =>
          {
            :doc => generic_slide_doc(:freq),
            :validations => [v_positive(:freq_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :mod_amp =>
          {
            :doc => "Amplitude of the modulation",
            :validations => [v_positive(:mod_amp)],
            :modulatable => true
          },

          :mod_amp_slide =>
          {
            :doc => generic_slide_doc(:mod_amp),
            :validations => [v_positive(:mod_amp_slide)],
            :modulatable => true,
            :bpm_scale => true
          }

        }
      end

      def doc
        "Attack of the Daleks! Ring mod is a classic effect often used on soundtracks to evoke robots or aliens as it sounds hollow or metallic. We take a 'carrier' signal (a sine wave controlled by the freq opt) and modulate its amplitude using the signal given inside the fx block. This produces a wide variety of sounds - the best way to learn is to experiment!"
      end
    end

    class FXBPF < FXInfo
      def name
        "Band Pass Filter"
      end

      def introduced
        Version.new(2,3,0)
      end

      def synth_name
        "fx_bpf"
      end

      def arg_defaults
        super.merge({
          :centre => 100,
          :centre_slide => 0,
          :centre_slide_shape => 1,
          :centre_slide_curve => 0,
          :res => 0.6,
          :res_slide => 0,
          :res_slide_shape => 1,
          :res_slide_curve => 0
        })
      end

      def specific_arg_info
        {
          :centre =>
          {
            :doc => "Centre frequency for the filter as a MIDI note.",
            :validations => [v_greater_than_oet(:centre, 0)],
            :modulatable => true,
            :midi => true
          },


          :centre_slide =>
          {
            :doc => generic_slide_doc(:centre),
            :validations => [v_positive(:centre_slide)],
            :modulatable => true,
            :bpm_scale => true
          },
        }
      end

      def doc
        "Combines low pass and high pass filters to only allow a 'band' of frequencies through. If the band is very narrow (a low res value like 0.0001) then the BPF will reduce the original sound, almost down to a single frequency (controlled by the centre opt).

  With higher values for res we can simulate other filters e.g. telephone lines, by cutting off low and high frequencies.

Use FX `:band_eq` with a negative db for the opposite effect - to attenuate a given band of frequencies."
      end
    end

    class FXRBPF < FXBPF
      def name
        "Resonant Band Pass Filter"
      end

      def introduced
        Version.new(2,3,0)
      end

      def synth_name
        "fx_rbpf"
      end

      def arg_defaults
        super.merge({
                      :res => 0.5,
                      :res_slide => 0,
                      :res_slide_shape => 1,
                      :res_slide_curve => 0
                    })
      end

      def doc
        "Like the Band Pass Filter but with a resonance (slight volume boost) around the target frequency. This can produce an interesting whistling effect, especially when used with larger values for the res opt."
      end
    end

    class FXNBPF < FXBPF
      def name
        "Normalised Band Pass Filter"
      end

      def introduced
        Version.new(2,3,0)
      end

      def synth_name
        "fx_nbpf"
      end

      def doc
        "Like the Band Pass Filter but normalised. The normaliser is useful here as some volume is lost when filtering the original signal."
      end
    end



    class FXNRBPF < FXRBPF
      def name
        "Normalised Resonant Band Pass Filter"
      end

      def introduced
        Version.new(2,3,0)
      end

      def synth_name
        "fx_nrbpf"
      end

      def doc
        "Like the Band Pass Filter but normalised, with a resonance (slight volume boost) around the target frequency. This can produce an interesting whistling effect, especially when used with larger values for the res opt.

  The normaliser is useful here as some volume is lost when filtering the original signal."
      end
    end

    class FXLPF < FXInfo
      def name
        "Low Pass Filter"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "fx_lpf"
      end



      def doc
        "Dampens the parts of the signal that are higher than the cutoff point (typically the crunchy fizzy harmonic overtones) and keeps the lower parts (typically the bass/mid of the sound). Choose a higher cutoff to keep more of the high frequencies/treble of the sound and a lower cutoff to make the sound more dull and only keep the bass."
      end



      def arg_defaults
        super.merge({
          :cutoff => 100,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,
        })
      end

      def specific_arg_info
        {


        }
      end
    end

    class FXRLPF < FXLPF
      def name
        "Resonant Low Pass Filter"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "fx_rlpf"
      end

      def arg_defaults
        super.merge({
          :cutoff => 100,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,
          :res => 0.5,
          :res_slide => 0,
          :res_slide_shape => 1,
          :res_slide_curve => 0,
        })
      end

      def specific_arg_info
        {


        }
      end

      def doc
        "Dampens the parts of the signal that are higher than the cutoff point (typically the crunchy fizzy harmonic overtones) and keeps the lower parts (typically the bass/mid of the sound). The resonant part of the resonant low pass filter emphasises/resonates the frequencies around the cutoff point. The amount of emphasis is controlled by the res opt with a higher res resulting in greater resonance. High amounts of resonance (rq ~1) can create a whistling sound around the cutoff frequency.

  Choose a higher cutoff to keep more of the high frequencies/treble of the sound and a lower cutoff to make the sound more dull and only keep the bass."
      end
    end

    class FXNormRLPF < FXRLPF
      def name
        "Normalised Resonant Low Pass Filter"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "fx_nrlpf"
      end
    end

    class FXHPF < FXInfo
      def name
        "High Pass Filter"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "fx_hpf"
      end

      def doc
        "Dampens the parts of the signal that are lower than the cutoff point (typically the bass of the sound) and keeps the higher parts (typically the crunchy fizzy harmonic overtones). Choose a lower cutoff to keep more of the bass/mid and a higher cutoff to make the sound more light and crispy."
      end

      def arg_defaults
        super.merge({
          :cutoff => 100,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0
        })
      end
    end

    class FXRHPF < FXHPF
      def name
        "Resonant High Pass Filter"
      end

      def doc
        "Dampens the parts of the signal that are lower than the cutoff point (typically the bass of the sound) and keeps the higher parts (typically the crunchy fizzy harmonic overtones). The resonant part of the resonant high pass filter emphasises/resonates the frequencies around the cutoff point. The amount of emphasis is controlled by the res opt with a higher res resulting in greater resonance. High amounts of resonance (rq ~1) can create a whistling sound around the cutoff frequency.

  Choose a lower cutoff to keep more of the bass/mid and a higher cutoff to make the sound more light and crispy."
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "fx_rhpf"
      end

      def arg_defaults
        super.merge({
          :cutoff => 100,
          :cutoff_slide => 0,
          :cutoff_slide_shape => 1,
          :cutoff_slide_curve => 0,
          :res => 0.5,
          :res_slide => 0,
          :res_slide_shape => 1,
          :res_slide_curve => 0
        })
      end

      def specific_arg_info
        {


        }
      end
    end

    class FXNormRHPF < FXRHPF
      def name
        "Normalised Resonant High Pass Filter"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "fx_nrhpf"
      end

      "A resonant high pass filter chained to a normaliser. Ensures that the signal is both filtered by a standard high pass filter and then normalised to ensure the amplitude of the final output is constant. A high pass filter will reduce the amplitude of the resulting signal (as some of the sound has been filtered out) the normaliser can compensate for this loss (although will also have the side effect of flattening all dynamics). See doc for hpf."
    end

    class FXBandEQ < FXInfo
      def name
        "Band EQ Filter"
      end

      def introduced
        Version.new(2,8,0)
      end

      def synth_name
        "fx_band_eq"
      end

      def doc
        "Attenuate or Boost a frequency band"
      end

      def arg_defaults
        super.merge({
          :freq => 100,
          :freq_slide => 0,
          :freq_slide_shape => 1,
          :freq_slide_curve => 0,
          :res => 0.6,
          :res_slide => 0,
          :res_slide_shape => 1,
          :res_slide_curve => 0,
          :db => 0.6,
          :db_slide => 0,
          :db_slide_shape => 1,
          :db_slide_curve => 0,
        })
      end

      def specific_arg_info
        {

          :freq =>
          {
            :doc => "Centre frequency of the band in MIDI.",
            :validations => [v_positive_not_zero(:freq)],
            :modulatable => true,
            :midi => true
          },

          :freq_slide =>
          {
            :doc => generic_slide_doc(:freq),
            :validations => [v_positive(:freq_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :res =>
          {
            :doc => "Width of the band as a value between 0 and 1",
            :validations => [v_positive(:res), v_less_than(:res, 1)],
            :modulatable => true
          },

          :res_slide =>
          {
            :doc => generic_slide_doc(:res),
            :validations => [v_positive(:res_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :db =>
          {
            :doc => "Amount of boost or attenuation of the frequency band. A positive value boosts frequencies in the band, a negative value attenuates them.",
            :modulatable => true
          },

          :db_slide =>
          {
            :doc => generic_slide_doc(:db),
            :validations => [v_positive(:db_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

        }
      end

    end

    class FXNormLPF < FXLPF
      def name
        "Normalised Low Pass Filter."
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "fx_nlpf"
      end

      def doc
        "A low pass filter chained to a normaliser. Ensures that the signal is both filtered by a standard low pass filter and then normalised to ensure the amplitude of the final output is constant. A low pass filter will reduce the amplitude of the resulting signal (as some of the sound has been filtered out) the normaliser can compensate for this loss (although will also have the side effect of flattening all dynamics). See doc for lpf."
      end
    end

    class FXNormHPF < FXHPF
      def name
        "Normalised High Pass Filter"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "fx_nhpf"
      end

      def doc
        "A high pass filter chained to a normaliser. Ensures that the signal is both filtered by a standard high pass filter and then normalised to ensure the amplitude of the final output is constant. A high pass filter will reduce the amplitude of the resulting signal (as some of the sound has been filtered out) the normaliser can compensate for this loss (although will also have the side effect of flattening all dynamics). See doc for hpf."
      end
    end

    class FXNormaliser < FXInfo
      def name
        "Normaliser"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "fx_normaliser"
      end

      def doc
        "Raise or lower amplitude of sound to a specified level. Evens out the amplitude of incoming sound across the frequency spectrum by flattening all dynamics."
      end

      def arg_defaults
        super.merge({
          :level => 1,
          :level_slide => 0,
          :level_slide_shape => 1,
          :level_slide_curve => 0
        })
      end

      def specific_arg_info
        {
          :level =>
          {
            :doc => "The peak output amplitude level at which to normalise the input.",
            :validations => [v_greater_than_oet(:level, 0)],
            :modulatable => true
          },

          :level_slide =>
          {
            :doc => generic_slide_doc(:level),
            :validations => [v_positive(:level_slide)],
            :modulatable => true,
            :bpm_scale => true
          }
        }
      end
    end


    class FXTanh < FXInfo
      def name
        "Hyperbolic Tangent"
      end

      def introduced
        Version.new(2,9,0)
      end


      def synth_name
        "fx_tanh"
      end

      def trigger_with_logical_clock?
        false
      end


      def doc
        "Forces all audio through a hyperbolic tangent function which has the effect of acting like distorted limiter. It works by folding loud signals back in on itself. The louder the input signal, the more folding occurs - resulting in increased strange harmonics and distortion. This folding also has the effect of limiting the outgoing signal, therefore to increase the output amplitude use the `amp:` opt and to increase the folding/distortion use the `pre_amp:` opt. "
      end

      def arg_defaults
        super.merge({
          :krunch => 5,
          :krunch_slide => 0,
          :krunch_slide_shape => 1,
          :krunch_slide_curve => 0,
        })
      end
      def specific_arg_info
        {
          :krunch =>
          {
            :doc => "Higher values progressively destroy the sound. Achieved through a balanced manipulation of pre_amp and amp such that the tanh is pushed harder with higher krunch values yet the overall amplitude stays similar.",
            :modulatable => true
          },

          :krunch_slide =>
          {
            :doc => generic_slide_doc(:krunch),
            :validations => [v_positive(:krunch_slide)],
            :modulatable => true,
            :bpm_scale => true
          }
        }
      end
    end

    class FXPitchShift < FXInfo
      def name
        "Pitch shift"
      end

      def introduced
        Version.new(2,5,0)
      end

      def synth_name
        "fx_pitch_shift"
      end

      def trigger_with_logical_clock?
        :t_minus_delta
      end

      def arg_defaults
        super.merge({
          :window_size => 0.2,
          :window_size_slide => 0,
          :window_size_slide_shape => 1,
          :window_size_slide_curve => 0,
          :pitch => 0,
          :pitch_slide => 0,
          :pitch_slide_shape => 1,
          :pitch_slide_curve => 0,
          :pitch_dis => 0.0,
          :pitch_dis_slide => 0,
          :pitch_dis_slide_shape => 1,
          :pitch_dis_slide_curve => 0,
          :time_dis => 0.0,
          :time_dis_slide => 0,
          :time_dis_slide_shape => 1,
          :time_dis_slide_curve => 0,
        })
      end

      def specific_arg_info
        {
          :pitch =>
          {
            :doc => "Pitch adjustment in semitones. 1 is up a semitone, 12 is up an octave, -12 is down an octave etc. Maximum upper limit of 24 (up 2 octaves). Lower limit of -72 (down 6 octaves). Decimal numbers can be used for fine tuning.",
            :validations => [v_greater_than_oet(:pitch, -72), v_less_than_oet(:pitch, 24)],
            :modulatable => true
          },

          :pitch_slide =>
          {
            :doc => generic_slide_doc(:pitch),
            :validations => [v_positive(:pitch_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :window_size =>
          {
            :doc => "Pitch shift works by chopping the input into tiny slices, then playing these slices at a higher or lower rate. If we make the slices small enough and overlap them, it sounds like the original sound with the pitch changed.

  The window_size is the length of the slices and is measured in seconds. It needs to be around 0.2 (200ms) or greater for pitched sounds like guitar or bass, and needs to be around 0.02 (20ms) or lower for percussive sounds like drum loops. You can experiment with this to get the best sound for your input.",
            :validations => [v_greater_than(:window_size, 0.00005)],
            :modulatable => true
          },

          :window_size_slide =>
          {
            :doc => generic_slide_doc(:window_size),
            :validations => [v_positive(:window_size_slide)],
            :modulatable => true,
            :bpm_scale => true
          },


          :pitch_dis =>
          {
            :doc => "Pitch dispersion - how much random variation in pitch to add. Using a low value like 0.001 can help to \"soften up\" the metallic sounds, especially on drum loops. To be really technical, pitch_dispersion is the maximum random deviation of the pitch from the pitch ratio (which is set by the pitch param)",
            :validations => [v_greater_than_oet(:pitch_dis, 0)],
            :modulatable => true
          },

          :pitch_dis_slide =>
          {
            :doc => generic_slide_doc(:pitch_dis),
            :validations => [v_positive(:pitch_dis_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :time_dis =>
          {
            :doc => "Time dispersion - how much random delay before playing each grain (measured in seconds). Again, low values here like 0.001 can help to soften up metallic sounds introduced by the effect. Large values are also fun as they can make soundscapes and textures from the input, although you will most likely lose the rhythm of the original. NB - This won't have an effect if it's larger than window_size.",
            :validations => [v_greater_than_oet(:time_dis, 0)],
            :modulatable => true
          },

          :time_dis_slide =>
          {
            :doc => generic_slide_doc(:time_dis),
            :validations => [v_positive(:time_dis_slide)],
            :modulatable => true,
            :bpm_scale => true
          },



        }
      end

      def doc
        "Changes the pitch of a signal without affecting tempo. Does this mainly through the pitch parameter which takes a midi number to transpose by. You can also play with the other params to produce some interesting textures and sounds."
      end
    end

    class FXDistortion < FXInfo
      def name
        "Distortion"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "fx_distortion"
      end

      def doc
        "Distorts the signal reducing clarity in favour of raw crunchy noise."
      end

      def arg_defaults
        super.merge({
          :distort => 0.5,
          :distort_slide => 0,
          :distort_slide_shape => 1,
          :distort_slide_curve => 0
        })
      end

      def specific_arg_info
        {
          :distort =>
          {
            :doc => "Amount of distortion to be applied (as a value between 0 and 1)",
            :validations => [v_greater_than_oet(:distort, 0), v_less_than(:distort, 1)],
            :modulatable => true
          },

          :distort_slide =>
          {
            :doc => generic_slide_doc(:distort),
            :validations => [v_positive(:distort_slide)],
            :modulatable => true,
            :bpm_scale => true
          }
        }
      end
    end



    class FXPan < FXInfo
      def name
        "Pan"
      end

      def introduced
        Version.new(2,0,0)
      end

      def synth_name
        "fx_pan"
      end

      def doc
        "Specify where in the stereo field the sound should be heard. A value of -1 for pan will put the sound in the left speaker, a value of 1 will put the sound in the right speaker and values in between will shift the sound accordingly."
      end

      def arg_defaults
        super.merge({
          :pan => 0,
          :pan_slide => 0,
          :pan_slide_shape => 1,
          :pan_slide_curve => 0,
        })
      end
    end

    class FXFlanger < FXInfo
      def name
        "Flanger"
      end

      def introduced
        Version.new(2,3,0)
      end

      def synth_name
        "fx_flanger"
      end

      def doc
        "Mix the incoming signal with a copy of itself which has a rate modulating faster and slower than the original. Creates a swirling/whooshing effect."
      end

      def arg_defaults
        super.merge({
          :phase => 4,
          :phase_slide => 0,
          :phase_slide_shape => 1,
          :phase_slide_curve => 0,
          :phase_offset => 0,
          :wave => 4,
          :invert_wave => 0,
          :stereo_invert_wave => 0,
          :delay => 5,
          :delay_slide => 0,
          :delay_slide_shape => 1,
          :delay_slide_curve => 0,
          :max_delay => 20,
          :depth => 5,
          :depth_slide => 0,
          :depth_slide_shape => 1,
          :depth_slide_curve => 0,
          :decay => 2,
          :decay_slide => 0,
          :decay_slide_shape => 1,
          :decay_slide_curve => 0,
          :feedback => 0,
          :feedback_slide => 0,
          :feedback_slide_shape => 1,
          :feedback_slide_curve => 0,
          :invert_flange => 0
        })
      end

      def specific_arg_info
        {


          :phase =>
          {
            :doc => "Phase duration in beats of flanger modulation.",
            :validations => [v_positive_not_zero(:phase)],
            :modulatable => true,
            :bpm_scale => true
          },

          :phase_slide =>
          {
            :doc => generic_slide_doc(:phase),
            :validations => [v_positive(:phase_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :wave =>
          {
            :doc => "Wave type - 0 saw, 1 pulse, 2 triangle, 3 sine, 4 cubic. Different waves will produce different flanging modulation effects.",
            :validations => [v_one_of(:wave, [0, 1, 2, 3, 4])],
            :modulatable => true
          },

          :invert_wave =>
          {
            :doc => "Invert flanger control waveform (i.e. flip it on the y axis). 0=uninverted wave, 1=inverted wave.",
            :validations => [v_one_of(:invert_wave, [0, 1])],
            :modulatable => true
          },

          :stereo_invert_wave =>
          {
            :doc => "Make the flanger control waveform in the left ear an inversion of the control waveform in the right ear. 0=uninverted wave, 1=inverted wave. This happens after the standard wave inversion with param :invert_wave.",
            :validations => [v_one_of(:stereo_invert_wave, [0, 1])],
            :modulatable => true
          },

          :delay =>
          {
            :doc => "Amount of delay time between original and flanged version of audio.",
            :modulatable => true
          },

          :delay_slide =>
          {
            :doc => generic_slide_doc(:delay),
            :validations => [v_positive(:delay_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :max_delay =>
          {
            :doc => "Max delay time. Used to set internal buffer size.",
            :validations => [v_positive(:max_delay)],
            :modulatable => false
          },

          :depth =>
          {
            :doc => "Flange depth - greater depths produce a more prominent effect.",
            :modulatable => true
          },

          :depth_slide =>
          {
            :doc => generic_slide_doc(:depth),
            :validations => [v_positive(:depth_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :decay =>
          {
            :doc => "Flange decay time in ms",
            :validations => [v_positive(:decay)],
            :modulatable => true
          },

          :decay_slide =>
          {
            :doc => generic_slide_doc(:decay),
            :validations => [v_positive(:decay_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :feedback =>
          {
            :doc => "Amount of feedback.",
            :validations => [v_between_inclusive(:feedback, 0, 1)],
            :modulatable => true
          },

          :feedback_slide =>
          {
            :doc => generic_slide_doc(:feedback),
            :validations => [v_positive(:feedback_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :invert_flange =>
          {
            :doc => "Invert flanger signal. 0=no inversion, 1=inverted signal.",
            :validations => [v_one_of(:invert_flange, [0, 1])],
            :modulatable => true
          }

        }
      end
    end

    class FXTremolo < FXInfo
      def name
        "Tremolo"
      end

      def introduced
        Version.new(3,0,0)
      end

      def synth_name
        "fx_tremolo"
      end

      def doc
        "Modulate the volume of the sound."
      end

      def arg_defaults
        super.merge({
          :phase => 4,
          :phase_slide => 0,
          :phase_slide_shape => 1,
          :phase_slide_curve => 0,
          :phase_offset => 0,
          :wave => 2,
          :invert_wave => 0,
          :depth => 0.5,
          :depth_slide => 0,
          :depth_slide_shape => 1,
          :depth_slide_curve => 0
        })
      end

      def specific_arg_info
        {
          :phase =>
          {
            :doc => "Phase duration in beats of tremolo modulation.",
            :validations => [v_positive_not_zero(:phase)],
            :modulatable => true,
            :bpm_scale => true
          },

          :phase_slide =>
          {
            :doc => generic_slide_doc(:phase),
            :validations => [v_positive(:phase_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :wave =>
          {
            :doc => "Wave type - 0 saw, 1 pulse, 2 triangle, 3 sine, 4 cubic. Different waves will produce different tremolo modulation effects.",
            :validations => [v_one_of(:wave, [0, 1, 2, 3, 4])],
            :modulatable => true
          },

          :invert_wave =>
          {
            :doc => "Invert tremolo control waveform (i.e. flip it on the y axis). 0=uninverted wave, 1=inverted wave.",
            :validations => [v_one_of(:invert_wave, [0, 1])],
            :modulatable => true
          },

          :depth =>
          {
            :doc => "Tremolo depth - greater depths produce a more prominent effect.",
            :validations => [v_between_inclusive(:depth, 0, 1)],
            :modulatable => true
          },

          :depth_slide =>
          {
            :doc => generic_slide_doc(:depth),
            :validations => [v_positive(:depth_slide)],
            :modulatable => true,
            :bpm_scale => true
          }

        }
      end
    end

    class FXPingPong < FXInfo
      def name
        "Ping Pong Echo"
      end

      def introduced
        Version.new(3,2,0)
      end

      def synth_name
        "fx_ping_pong"
      end

      def doc
        "Echo FX with each delayed echo swapping between left and right channels. Has variable phase duration (time between echoes) and feedback (proportion of sound fed into each echo). If you wish to have a phase duration longer than 1s, you need to specify the longest phase duration you'd like with the arg max_phase. Be warned, `:ping_pong` FX with very long phases can consume a lot of memory and take longer to initialise. Also, large values for feedback will cause the echo to last for a very long time.

Note: sliding the `phase:` opt with `phase_slide:` will also cause each echo during the slide to change in pitch, in much the same way that a sample's pitch changes when altering its rate."
      end

      def arg_defaults
        super.merge({
          :phase => 0.25,
          :phase_slide => 0,
          :phase_slide_shape => 1,
          :phase_slide_curve => 0,
          :feedback => 0.5,
          :feedback_slide => 0,
          :feedback_slide_shape => 1,
          :feedback_slide_curve => 0,
          :max_phase => 1,
          :pan_start => 1
        })
      end

      def specific_arg_info
        {
          :phase =>
          {
            :doc => "The time between echoes in beats.",
            :validations => [v_positive_not_zero(:phase)],
            :modulatable => true,
            :bpm_scale => true
          },

          :phase_slide =>
          {
            :doc => generic_slide_doc(:phase),
            :validations => [v_positive(:phase_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :max_phase =>
          {
            :doc => "The maximum phase duration in beats.",
            :validations => [v_positive_not_zero(:max_phase)],
            :modulatable => false,
            :bpm_scale => true
          },

          :feedback =>
          {
            :doc => "Proportion of sound fed into each successive echo from the previous one.",
            :validations => [v_positive_not_zero(:feedback), v_less_than(:feedback, 1)],
            :modulatable => true
          },

          :feedback_slide =>
          {
            :doc => generic_slide_doc(:feedback),
            :validations => [v_positive(:feedback_slide)],
            :modulatable => true,
            :bpm_scale => true
          },

          :pan_start =>
          {
            :doc => "Starting position of sound in the stereo field. With headphones on, this means how much of the sound starts in the left ear, and how much starts in the right ear. With a value of -1, the sound starts completely in the left ear, a value of 0 starts the sound equally in both ears, and a value of 1 starts the sound completely in the right ear. Values in between -1 and 1 move the sound accordingly. Each echo will swap between left and right at the same distance away from 0 (the centre) that this `pan_start:` opt is set to. For example, with a value of -1, the sound starts completely in the left ear, and the echoes after this will swap between fully right and fully left (1 and -1). With a value of 0, since the sound starts in the centre of the stereo field, each echo also stays in the centre, meaning the panning effect is cancelled out.",
            :validations => [v_between_inclusive(:pan_start, -1, 1)],
            :modulatable => false
          }
        }
      end

      def kill_delay(args_h)
        feedback = args_h[:feedback] || arg_defaults[:feedback]
        phase = args_h[:phase] || arg_defaults[:phase]
        (Math.log(0.01) / Math.log(feedback)) * phase
      end
    end

    class BaseInfo

      @@grouped_samples =
        {
        :drum => {
          :desc => "Drum Sounds",
          :prefix => "drum_",
          :samples => [
            :drum_heavy_kick,
            :drum_tom_mid_soft,
            :drum_tom_mid_hard,
            :drum_tom_lo_soft,
            :drum_tom_lo_hard,
            :drum_tom_hi_soft,
            :drum_tom_hi_hard,
            :drum_splash_soft,
            :drum_splash_hard,
            :drum_snare_soft,
            :drum_snare_hard,
            :drum_cymbal_soft,
            :drum_cymbal_hard,
            :drum_cymbal_open,
            :drum_cymbal_closed,
            :drum_cymbal_pedal,
            :drum_bass_soft,
            :drum_bass_hard,
            :drum_cowbell,
            :drum_roll]},

        :elec => {
          :desc => "Electric Sounds",
          :prefix => "elec_",
          :samples => [
            :elec_triangle,
            :elec_snare,
            :elec_lo_snare,
            :elec_hi_snare,
            :elec_mid_snare,
            :elec_cymbal,
            :elec_soft_kick,
            :elec_filt_snare,
            :elec_fuzz_tom,
            :elec_chime,
            :elec_bong,
            :elec_twang,
            :elec_wood,
            :elec_pop,
            :elec_beep,
            :elec_blip,
            :elec_blip2,
            :elec_ping,
            :elec_bell,
            :elec_flip,
            :elec_tick,
            :elec_hollow_kick,
            :elec_twip,
            :elec_plip,
            :elec_blup]},

        :guit => {
          :desc => "Sounds featuring guitars",
          :prefix => "guit_",
          :samples => [
            :guit_harmonics,
            :guit_e_fifths,
            :guit_e_slide,
            :guit_em9]},

        :misc => {
          :desc => "Miscellaneous Sounds",
          :prefix => "misc_",
          :samples => [
            :misc_burp,
            :misc_crow,
            :misc_cineboom]},

        :perc => {
          :desc => "Percussive Sounds",
          :prefix => "perc_",
          :samples => [
            :perc_bell,
            :perc_bell2,
            :perc_snap,
            :perc_snap2,
            :perc_swash,
            :perc_till,
            :perc_door,
            :perc_impact1,
            :perc_impact2,
            :perc_swoosh]},

        :ambi => {
          :desc => "Ambient Sounds",
          :prefix => "ambi_",
          :samples => [
            :ambi_soft_buzz,
            :ambi_swoosh,
            :ambi_drone,
            :ambi_glass_hum,
            :ambi_glass_rub,
            :ambi_haunted_hum,
            :ambi_piano,
            :ambi_lunar_land,
            :ambi_dark_woosh,
            :ambi_choir,
            :ambi_sauna]},

        :bass => {
          :desc => "Bass Sounds",
          :prefix => "bass_",
          :samples => [
            :bass_hit_c,
            :bass_hard_c,
            :bass_thick_c,
            :bass_drop_c,
            :bass_woodsy_c,
            :bass_voxy_c,
            :bass_voxy_hit_c,
            :bass_dnb_f]},

        :sn => {
          :desc => "Snare Drums",
          :prefix => "sn_",
          :samples => [
            :sn_dub,
            :sn_dolf,
            :sn_zome,
            :sn_generic]},

        :bd => {
          :desc => "Bass Drums",
          :prefix => "bd_",
          :samples => [
            :bd_ada,
            :bd_pure,
            :bd_808,
            :bd_zum,
            :bd_gas,
            :bd_sone,
            :bd_haus,
            :bd_zome,
            :bd_boom,
            :bd_klub,
            :bd_fat,
            :bd_tek,
            :bd_mehackit]},

        :loop => {
          :desc => "Sounds for Looping",
          :prefix => "loop_",
          :samples => [
            :loop_industrial,
            :loop_compus,
            :loop_amen,
            :loop_amen_full,
            :loop_garzul,
            :loop_mika,
            :loop_breakbeat,
            :loop_safari,
            :loop_tabla,
            :loop_3d_printer,
            :loop_drone_g_97,
            :loop_electric,
            :loop_mehackit1,
            :loop_mehackit2,
            :loop_perc1,
            :loop_perc2,
            :loop_weirdo
          ]},

        :tabla => {
          :desc => "Sounds of a Tabla Drum",
          :prefix => "tabla_",
          :samples => [
            :tabla_tas1,
            :tabla_tas2,
            :tabla_tas3,
            :tabla_ke1,
            :tabla_ke2,
            :tabla_ke3,
            :tabla_na,
            :tabla_na_o,
            :tabla_tun1,
            :tabla_tun2,
            :tabla_tun3,
            :tabla_te1,
            :tabla_te2,
            :tabla_te_ne,
            :tabla_te_m,
            :tabla_ghe1,
            :tabla_ghe2,
            :tabla_ghe3,
            :tabla_ghe4,
            :tabla_ghe5,
            :tabla_ghe6,
            :tabla_ghe7,
            :tabla_ghe8,
            :tabla_dhec,
            :tabla_na_s,
            :tabla_re]},

        :glitch => {
          :desc => "Glitchy Sounds",
          :prefix => "glitch_",
          :samples => [
            :glitch_bass_g,
            :glitch_perc1,
            :glitch_perc2,
            :glitch_perc3,
            :glitch_perc4,
            :glitch_perc5,
            :glitch_robot1,
            :glitch_robot2]},

        :vinyl => {
          :desc => "Vinyl sounds",
          :prefix => "vinyl_",
          :samples => [
            :vinyl_backspin,
            :vinyl_rewind,
            :vinyl_scratch,
            :vinyl_hiss]},

        :mehackit => {
          :desc => "Mehackit Sounds",
          :prefix => "mehackit_",
          :samples => [
            :mehackit_phone1,
            :mehackit_phone2,
            :mehackit_phone3,
            :mehackit_phone4,
            :mehackit_robot1,
            :mehackit_robot2,
            :mehackit_robot3,
            :mehackit_robot4,
            :mehackit_robot5,
            :mehackit_robot6,
            :mehackit_robot7
          ]},
        }


      @@all_samples = (@@grouped_samples.values.reduce([]) {|s, el| s << el[:samples]}).flatten

      @@synth_infos =
        {
        :dull_bell => DullBell.new,
        :pretty_bell => PrettyBell.new,
        :beep => Beep.new,
        :sine => Beep.new,
        :saw => Saw.new,
        :pulse => Pulse.new,
        :subpulse => SubPulse.new,
        :square => Square.new,
        :tri => Tri.new,
        :dsaw => DSaw.new,
        :dpulse => DPulse.new,
        :dtri => DTri.new,
        :fm => FM.new,
        :mod_fm => ModFM.new,
        :mod_saw => ModSaw.new,
        :mod_dsaw => ModDSaw.new,
        :mod_sine => ModSine.new,
        :mod_beep => ModSine.new,
        :mod_tri => ModTri.new,
        :mod_pulse => ModPulse.new,
        :chiplead => ChipLead.new,
        :chipbass => ChipBass.new,
        :tb303 => TB303.new,
        :supersaw => Supersaw.new,
        :hoover => Hoover.new,
        :prophet => Prophet.new,
        :zawa => Zawa.new,
        :dark_ambience => DarkAmbience.new,
        :growl => Growl.new,
        :hollow => Hollow.new,
        #      :dark_sea_horn => DarkSeaHorn.new,
        #      :singer        => Singer.new,
        :mono_player => MonoPlayer.new,
        :stereo_player => StereoPlayer.new,
        :blade => SynthViolin.new,
        :piano => SynthPiano.new,
        :rodeo => SynthRodeo.new,
        :kalimba => SynthKalimba.new,
        :pluck => SynthPluck.new,
        :tech_saws => TechSaws.new,

        :sound_in => SoundIn.new,
        :sound_in_stereo => SoundInStereo.new,
        :noise => Noise.new,
        :pnoise => PNoise.new,
        :bnoise => BNoise.new,
        :gnoise => GNoise.new,
        :cnoise => CNoise.new,
        :chipnoise => ChipNoise.new,

        :basic_mono_player => BasicMonoPlayer.new,
        :basic_stereo_player => BasicStereoPlayer.new,
        :basic_mixer => BasicMixer.new,
        :main_mixer => MainMixer.new,

        :fx_bitcrusher => FXBitcrusher.new,
        :fx_krush => FXKrush.new,
        :fx_reverb => FXReverb.new,
        :fx_gverb => FXGVerb.new,
        :fx_replace_reverb => FXReverb.new,
        :fx_level => FXLevel.new,
        :fx_mono => FXMono.new,
        :fx_autotuner => FXAutotuner.new,
        :fx_replace_level => FXLevel.new,
        :fx_echo => FXEcho.new,
        :fx_replace_echo => FXEcho.new,
        :fx_slicer => FXSlicer.new,
        :fx_panslicer => FXPanSlicer.new,
        :fx_replace_slicer => FXSlicer.new,
        :fx_wobble => FXWobble.new,
        :fx_replace_wobble => FXWobble.new,
        :fx_ixi_techno => FXIXITechno.new,
        :fx_replace_ixi_techno => FXIXITechno.new,
        :fx_compressor => FXCompressor.new,
        :fx_whammy => FXWhammy.new,
        :fx_replace_compressor => FXCompressor.new,
        :fx_rlpf => FXRLPF.new,
        :fx_replace_rlpf => FXRLPF.new,
        :fx_nrlpf => FXNormRLPF.new,
        :fx_replace_nrlpf => FXNormRLPF.new,
        :fx_rhpf => FXRHPF.new,
        :fx_replace_rhpf => FXRHPF.new,
        :fx_nrhpf => FXNormRHPF.new,
        :fx_replace_nrhpf => FXNormRHPF.new,
        :fx_hpf => FXHPF.new,
        :fx_replace_hpf => FXHPF.new,
        :fx_nhpf => FXNormHPF.new,
        :fx_replace_nhpf => FXNormHPF.new,
        :fx_lpf => FXLPF.new,
        :fx_replace_lpf => FXLPF.new,
        :fx_nlpf => FXNormLPF.new,
        :fx_replace_nlpf => FXNormLPF.new,
        :fx_normaliser => FXNormaliser.new,
        :fx_replace_normaliser => FXNormaliser.new,
        :fx_distortion => FXDistortion.new,
        :fx_replace_distortion => FXDistortion.new,
        :fx_pan => FXPan.new,
        :fx_replace_pan => FXPan.new,
        :fx_bpf => FXBPF.new,
        :fx_nbpf => FXNBPF.new,
        :fx_rbpf => FXRBPF.new,
        :fx_nrbpf => FXNRBPF.new,
        :fx_band_eq => FXBandEQ.new,
        :fx_tanh => FXTanh.new,
        :fx_pitch_shift => FXPitchShift.new,
        :fx_ring_mod => FXRingMod.new,
        #:fx_chorus => FXChorus.new,
        :fx_octaver => FXOctaver.new,
        :fx_vowel => FXVowel.new,
        :fx_flanger => FXFlanger.new,
        :fx_eq => FXEQ.new,
        :fx_tremolo => FXTremolo.new,
        :fx_record => FXRecord.new,
        :fx_sound_out => FXSoundOut.new,
        :fx_sound_out_stereo => FXSoundOutStereo.new,
        :fx_ping_pong => FXPingPong.new
      }

      def self.get_info(synth_name)
        @@synth_infos[synth_name.to_sym]
      end

      def self.get_all
        @@synth_infos
      end

      def self.grouped_samples
        @@grouped_samples
      end

      def self.all_samples
        @@all_samples
      end

      def self.all_synths
        @@synth_infos.select {|k, v| v.is_a?(SonicPiSynth) && v.user_facing?}.keys
      end

      def self.all_fx
        fx = @@synth_infos.select {|k, v| v.is_a?(FXInfo) && v.user_facing? && !k.to_s.include?('replace_')}.keys
        fx.map { |k, v| k.to_s[3..-1].to_sym }
      end

      def self.info_doc_html_map(klass)
        res = {}

        max_len =  0
        get_all.each do |k, v|
          next unless v.is_a? klass
          next if (klass == FXInfo) && (k.to_s.include? 'replace_')
          next if v.is_a? StudioInfo
          if klass == SynthInfo
            max_len = k.to_s.size if k.to_s.size > max_len
          else
            max_len = (k.to_s.size - 3) if (k.to_s.size - 3) > max_len
          end
        end

        get_all.each do |k, v|
          next unless v.is_a? klass
          next if (klass == FXInfo) && (k.to_s.include? 'replace_')

          next if v.is_a? StudioInfo
          doc = "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n\n"
          doc << "<body class=\"manual\">\n"
          doc << "<h1>" << v.name << "</h1>\n\n"

          doc << "<p><table class=\"arguments\"><tr>\n"
          cnt = 0
          v.arg_info.each do |ak, av|
            doc << "</tr><tr>" if (cnt > 0) and cnt % 4 == 0
            doc << "<td class=\"even\"><a href=\"##{ak}\">#{ak}:</a></td>\n"
            doc << "<td class=\"odd\">#{av[:default]}</td>\n"
            cnt += 1
          end
          doc << "</tr></table></p>\n\n"

          doc << "<p class=\"usage\"><code><pre>"
          if klass == SynthInfo
            safe_k = k
            doc << "use_synth <span class=\"symbol\">:#{safe_k}</span>"
          else
            safe_k = k.to_s[3..-1]
            doc << "with_fx <span class=\"symbol\">:#{safe_k}</span> <span class=\"keyword\">do</span>\n"
            doc << "  play <span class=\"number\">50</span>\n"
            doc << "<span class=\"keyword\">end</span>"
          end
          doc << "</pre></code></p>\n"

          doc << Kramdown::Document.new(v.doc).to_html << "\n"

          doc << "<p class=\"introduced\">"
          doc << "Introduced in " << v.introduced.to_s << "</p>\n\n"

          doc << "<h2>Options</h2>\n"

          doc << "<p><table class=\"details\">\n"

          cnt = 0
          any_slidable = false
          v.arg_info.each do |ak, av|
            td_class = cnt.even? ? "even" : "odd"
            doc << "<a name=\"#{ak}\"></a>\n"
            doc << "<tr>\n"
            doc << " <td class=\"#{td_class} key\">#{ak}:</td>\n"
            doc << " <td class=\"#{td_class}\">\n"
            docstring = av[:doc] || 'write me'
            doc <<  Kramdown::Document.new(docstring).to_html
            doc << "  <p class=\"properties\">\n"
            doc << "   Default: #{av[:default]}\n"
            doc << "   <br/>#{av[:constraints].join(",").capitalize}\n" unless av[:constraints].empty?
            doc << "   <br/>#{av[:modulatable] ? "May be changed whilst playing" : "Can not be changed once set"}\n"
            doc << "   <br/><a href=\"#slide\">Has slide options to shape changes</a>\n" if av[:slidable]
            doc << "   <br/>Scaled with current BPM value\n" if av[:bpm_scale]
            doc << "  </p>\n"
            doc << " </td>\n"
            doc << "</tr>\n"
            any_slidable = true if av[:slidable]
            cnt += 1
          end
          doc << "</table></p>\n"

          if any_slidable then
          doc << slide_doc_html(v)
          end # any_slidable

          doc << "</body>\n"

          res["#{safe_k}"] = doc
        end
        res
      end

      def self.info_doc_markdown(name, klass, key_mod=nil)
        res = "# #{name}\n\n"

        get_all.each do |k, v|
          next unless v.is_a? klass
          next if k.to_s.include? 'replace_'
          snake_case = v.name.downcase.gsub(/ /, "-")
          res << "* [#{v.name}](##{snake_case})\n"
        end
        res << "\n"
        get_all.each do |k, v|
          next unless v.is_a? klass
          next if k.to_s.include? 'replace_'
          res << "## " << v.name << "\n\n"
          res << "### Key:\n"
          mk = key_mod ? key_mod.call(k) : k
          res << "  :#{mk}\n\n"
          res << "### Doc:\n"
          res << "  " << v.doc << "\n\n"
          res << "### Opts:" "\n"
          v.arg_info.each do |ak, av|
            res << "  * #{ak}:\n"
            res << "    - doc: #{av[:doc] || 'write me'}\n"
            res << "    - default: #{av[:default]}\n"
            res << "    - constraints: #{av[:constraints].empty? ? "none" : av[:constraints].join(",")}\n"
            res << "    - #{av[:modulatable] ? "May be changed whilst playing" : "Can not be changed once set"}\n"
            res << "    - Scaled with current BPM value\n" if av[:bpm_scale]
            res << "    - Accepts note symbols such as :e3\n" if av[:midi]
            res << "    - Has slide options for shaping changes\n" if av[:slidable]
          end
          res << "\n\n"

        end
        res
      end

      def self.synth_doc_html_map
        info_doc_html_map(SynthInfo)
      end

      def self.fx_doc_html_map
        info_doc_html_map(FXInfo)
      end

      def self.synth_doc_markdown
        info_doc_markdown("Synths", SynthInfo)
      end

      def self.fx_doc_markdown
        info_doc_markdown("FX", FXInfo, lambda{|k| k.to_s[3..-1]})
      end

      def self.samples_doc_html_map
        res = {}

        grouped_samples.each do |k, v|
          cnt = 0
          cnt = 0
          doc = "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n\n"
          doc << "<body class=\"manual\">\n\n"
          doc << "<h1>" << v[:desc] << "</h1>\n"


          doc << "<table>\n"

          v[:samples].each do |s|
            doc << "<tr style=\"vertical-align: middle;padding:5px\">\n"
            doc << "<td><a href=\"sonicpi://play-sample/#{s}\"><img src=\":/images/play.png\" width=\"15\" height=\"16\"></a></td>\n"
            doc << "<td><p class=\"usage\"><code><pre> sample <span class=\"symbol\">:#{s}</span> </pre></code></p></td>\n"
            doc << "</tr>\n"
          end
          doc << "</table>\n"

          doc << "<p><table class=\"arguments\"><tr>\n"

          stereo_player = StereoPlayer.new
          stereo_player.arg_info.each do |ak, av|
            doc << "</tr><tr>" if (cnt > 0) and cnt % 4 == 0
            doc << "<td class=\"even\"><a href=\"##{ak}\">#{ak}:</a></td>\n"
            doc << "<td class=\"odd\">#{av[:default]}</td>\n"
            cnt += 1
          end
          doc << "</tr></table></p>\n"

          doc << "<p><table class=\"details\">\n"

          cnt = 0
          any_slidable = false
          stereo_player.arg_info.each do |ak, av|
            doc << "<a name=\"#{ak}\"></a>\n"
            doc << "<tr>\n"
            doc << " <td class=\"even key\">#{ak}:</td>\n"
            doc << " <td class=\"odd\">\n"
            doc << "  <p>#{av[:doc] || 'write me'}</p>\n"
            doc << "  <p class=\"properties\">\n"
            doc << "   Default: #{av[:default]}\n"
            doc << "   <br/>#{av[:constraints].join(",")}\n" unless av[:constraints].empty?
            if av[:slidable]
              doc << "   <br/>May be changed whilst playing\n"
              doc << "   <br/><a href=\"#slide\">Has slide options to shape changes</a>\n"
              any_slidable = true
            end
            doc << "   <br/>Scaled with current BPM value\n" if av[:bpm_scale]
            doc << "  </p>\n"
            doc << " </td>\n"
            doc << "</tr>\n"
            cnt += 1
          end
          doc << "</table></p>\n"
          doc << slide_doc_html(stereo_player) if any_slidable
          doc << "</body>\n"

          res[v[:desc]] = doc
        end
        res
      end

      def self.samples_doc_markdown
        res = "# Samples\n\n"
        grouped_samples.values.each do |info|
          res << "## #{info[:desc]}\n"
          info[:samples].each do |s|
            res << "* :#{s}\n"
          end
          res << "\n\n"

        end
        res
      end

      def self.slide_doc_html(synth)
        slide_doc = ""
        slide_doc << "<a name=slide></a>\n"
        slide_doc << "<h2>Slide Options</h2>\n"
        slide_doc << "<p>Any parameter that is slidable has three additional options named _slide, _slide_curve, and _slide_shape.  For example, 'amp' is slidable, so you can also set amp_slide, amp_slide_curve, and amp_slide_shape with the following effects:</p>\n"
        slide_args = {
          :_slide => {:default => 0, :doc=>synth.generic_slide_doc('parameter')},
          :_slide_shape => {:default=>5, :doc=>synth.generic_slide_shape_doc('parameter')},
          :_slide_curve => {:default=>0, :doc=>synth.generic_slide_curve_doc('parameter')}
        }

        # table for slide parameters
        slide_doc << "<p><table class=\"details\">\n"

        cnt = 0
        slide_args.each do |ak, av|
          td_class = cnt.even? ? "even" : "odd"
          slide_doc << "<tr>\n"
          slide_doc << " <td class=\"#{td_class} key\">#{ak}:</td>\n"
          slide_doc << " <td class=\"#{td_class}\">\n"
          slide_doc << "  <p>#{av[:doc] || 'write me'}</p>\n"
          slide_doc << "  <p class=\"properties\">\n"
          slide_doc << "   Default: #{av[:default]}\n"
          slide_doc << "  </p>\n"
          slide_doc << " </td>\n"
          slide_doc << "</tr>\n"
          cnt += 1
        end
        slide_doc << "</table></p>\n"
        slide_doc
      end
    end
  end
end
