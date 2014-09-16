#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, distribution,
# and distribution of modified versions of this work as long as this
# notice is included.
#++
require_relative "version"

module SonicPi

  class BaseInfo
    attr_reader :scsynth_name, :info

    def initialize
      @scsynth_name = "#{prefix}#{synth_name}"
      @info = default_arg_info.merge(specific_arg_info)
    end

    def rrand(min, max)
      range = (min - max).abs
      r = rand(range.to_f)
      smallest = [min, max].min
      r + smallest
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

    def validate!(*args)
      args_h = resolve_synth_opts_hash_or_array(args)

      args_h.each do |k, v|
        k_sym = k.to_sym
#        raise "Value of argument #{k_sym.inspect} must be a number, got #{v.inspect}." unless v.is_a? Numeric

        arg_validations(k_sym).each do |v_fn, msg|
          raise "Value of argument #{k_sym.inspect} #{msg}, got #{v.inspect}." unless v_fn.call(args_h)
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

    def arg_info
      #Specifically for doc usage. Consider changing name do doc_info
      #Don't call as part of audio loops as slow. Use .info directly
      res = {}
      arg_defaults.each do |arg, default|
        default_info = @info[arg] || {}
        constraints = (default_info[:validations] || []).map{|el| el[1]}
        new_info = {}
        new_info[:doc] = default_info[:doc]
        new_info[:default] = default
        new_info[:constraints] = constraints
        new_info[:modulatable] = default_info[:modulatable]
        res[arg] = new_info
      end

      res

    end

    def kill_delay(args_h)
      1
    end

    private

    def generic_slide_doc(k)
      return "Amount of time (in seconds) for the #{k} value to change. A long #{k}_slide value means that the #{k} takes a long time to slide from the previous value to the new value. A #{k}_slide of 0 means that the #{k} instantly changes to the new value."
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
        :mix =>
        {
          :doc => "The amount (percentage) of FX present in the resulting sound represented as a value between 0 and 1. For example, a mix of 0 means that only the original sound is heard, a mix of 1 means that only the FX is heard (typically the default) and a mix of 0.5 means that half the original and half of the FX is heard. ",
          :validations => [v_between_inclusive(:mix, 0, 1)],
          :modulatable => true
        },

        :mix_slide =>
        {
          :doc => "Amount of time (in seconds) for the mix value to change. A long slide value means that the mix takes a long time to slide from the previous value to the new value. A slide of 0 means that the mix instantly changes to the new value.",
          :validations => [v_positive(:mix_slide)],
          :modulatable => true
        },

        :note =>
        {
          :doc => "Note to play. Either a MIDI number or a symbol representing a note. For example: 30, 52, :C, :C2, :Eb4, or :Ds3",
          :validations => [v_positive(:note)],
          :modulatable => true
        },

        :note_slide =>
        {
          :doc => "Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.",
          :validations => [v_positive(:note_slide)],
          :modulatable => true,
          :bpm_scale => true
        },

        :amp =>
        {
          :doc => "The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)",
          :validations => [v_positive(:amp)],
          :modulatable => true
        },

        :amp_slide =>
        {
          :doc => "Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.",
          :validations => [v_positive(:amp_slide)],
          :modulatable => true,
          :bpm_scale => true
        },

        :pan =>
        {

          :doc => "Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.",
          :validations => [v_between_inclusive(:pan, -1, 1)],
          :modulatable => true
        },

        :pan_slide =>
        {
          :doc => "Amount of time (in seconds) for the pan to change. A long slide value means that the pan takes a long time to slide from the previous pan position to the new pan position. A slide of 0 means that the pan instantly changes to the new pan position.",
          :validations => [v_positive(:pan_slide)],
          :modulatable => true,
          :bpm_scale => true
        },


        :attack =>
        {
          :doc => "Amount of time (in seconds) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.",
          :validations => [v_positive(:attack)],
          :modulatable => false,
          :bpm_scale => true
        },

        :decay =>
        {
          :doc => "Amount of time (in seconds) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).",
          :validations => [v_positive(:decay)],
          :modulatable => false,
          :bpm_scale => true
        },

        :sustain =>
        {
          :doc => "Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.",
          :validations => [v_positive(:sustain)],
          :modulatable => false,
          :bpm_scale => true
        },

        :release =>
        {
          :doc => "Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.",
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
          :modulatable => true
        },

        :cutoff_slide =>
        {
          :doc => "Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value.",
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
          :doc => "Phase duration in seconds of oscillations between the two notes. Time it takes to switch betwen the notes.",
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
          :validations => [v_positive(:mod_range)],
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
          :doc => "Filter resonance. Large amounts of resonance (a res: near 0) can create a whistling sound around the cutoff frequency. Smaller values produce more resonance.",
          :validations => [v_positive_not_zero(:res)],
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
          :doc => "Time in seconds for pulse width to change.",
          :validations => [v_positive(:pulse_width_slide)],
          :modulatable => true,
          :bpm_scale => true
        },

        :mod_pulse_width =>
        {
          :doc => "The width of the modualted pulse wave as a value between 0 and 1. A width of 0.5 will produce a square wave. Only valid if mod wave is type pulse.",
          :validations => [v_between_exclusive(:mod_pulse_width, 0, 1)],
          :modulatable => true
        },

        :mod_pulse_width_slide =>
        {
          :doc => "Time in seconds for modulated pulse width to change.",
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
      "A simple dull dischordant bell sound."
    end

    def arg_defaults
      {
        :note => 52,
        :note_slide => 0,
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,

        :attack => 0,
        :decay => 0,
        :sustain => 0,
        :release => 1,
        :attack_level => 1,
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
      "A pretty bell sound. Works well with short attacks and long delays."
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
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,

        :attack => 0,
        :decay => 0,
        :sustain => 0,
        :release => 1,
        :attack_level => 1,
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
  end


  class Pulse < SonicPiSynth
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
      "A simple pulse wave with a low pass filter. This defaults to a square wave, but the timbre can be changed dramatically by adjusting the pulse_width arg between 0 and 1."
    end

    def arg_defaults
      {
        :note => 52,
        :note_slide => 0,
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,

        :attack => 0,
        :decay => 0,
        :sustain => 0,
        :release => 1,
        :attack_level => 1,
        :sustain_level => 1,
        :env_curve => 2,

        :cutoff => lambda{rrand(95, 105)},
        :cutoff_slide => 0,
        :pulse_width => 0.5,
        :pulse_width_slide => 0
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
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,

        :attack => 0,
        :decay => 0,
        :sustain => 0,
        :release => 1,
        :attack_level => 1,
        :sustain_level => 1,
        :env_curve => 2,

        :cutoff => 100,
        :cutoff_slide => 0,
        :detune => 0.1,
        :detune_slide => 0
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
      "A sine wave with a fundamental frequency which is modulated at audio rate by another sine wave with a specific modulation division and depth. Useful for generated a wide range of sounds by playing with the divisor and depth params. Great for deep powerful bass and crazy 70s sci-fi sounds."
    end

    def arg_defaults
      {
        :note => 52,
        :note_slide => 0,
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,

        :attack => 0,
        :decay => 0,
        :sustain => 0,
        :release => 1,
        :attack_level => 1,
        :sustain_level => 1,
        :env_curve => 2,

        :divisor => 2,
        :divisor_slide => 0,
        :depth => 1,
        :depth_slide => 0
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
      "The FM synth modulating between two notes - the duration of the modulation can be modified using the mod_phase arg, the range (number of notes jumped between) by the mod_range arg and the width of the jumps by the mod_width param. The FM synth is sine wave with a fundamental frequency which is modulated at audio rate by another sine wave with a specific modulation division and depth. Useful for generated a wide range of sounds by playing with the divisor and depth params. Great for deep powerful bass and crazy 70s sci-fi sounds."
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
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,

        :attack => 0,
        :decay => 0,
        :sustain => 0,
        :release => 1,
        :attack_level => 1,
        :sustain_level => 1,
        :env_curve => 2,

        :cutoff => 100,
        :cutoff_slide => 0,
        :mod_phase => 0.25,
        :mod_phase_slide => 0,
        :mod_range => 5,
        :mod_range_slide => 0,
        :mod_pulse_width => 0.5,
        :mod_pulse_width_slide => 0,
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
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,

        :attack => 0,
        :decay => 0,
        :sustain => 0,
        :release => 1,
        :attack_level => 1,
        :sustain_level => 1,
        :env_curve => 2,

        :cutoff => 100,
        :cutoff_slide => 0,
        :mod_phase => 0.25,

        :mod_phase_slide => 0,
        :mod_range => 5,
        :mod_range_slide => 0,
        :mod_pulse_width => 0.5,
        :mod_pulse_width_slide => 0,
        :mod_phase_offset => 0,
        :mod_invert_wave => 0,
        :mod_wave => 1,
        :detune => 0.1,
        :detune_slide => 0
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
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,

        :attack => 0,
        :decay => 0,
        :sustain => 0,
        :release => 1,
        :attack_level => 1,
        :sustain_level => 1,
        :env_curve => 2,

        :cutoff => 100,
        :cutoff_slide => 0,
        :mod_phase => 0.25,
        :mod_phase_slide => 0,
        :mod_range => 5,
        :mod_range_slide => 0,
        :mod_pulse_width => 0.5,
        :mod_pulse_width_slide => 0,
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
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,

        :attack => 0,
        :decay => 0,
        :sustain => 0,
        :release => 1,
        :attack_level => 1,
        :sustain_level => 1,
        :env_curve => 2,

        :cutoff => 100,
        :cutoff_slide => 0,
        :mod_phase => 0.25,
        :mod_phase_slide => 0,
        :mod_range => 5,
        :mod_range_slide => 0,
        :mod_pulse_width => 0.5,
        :mod_pulse_width_slide => 0,
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
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,

        :attack => 0,
        :decay => 0,
        :sustain => 0,
        :release => 1,
        :attack_level => 1,
        :sustain_level => 1,
        :env_curve => 2,

        :cutoff => 100,
        :cutoff_slide => 0,
        :mod_phase => 0.25,
        :mod_phase_slide => 0,
        :mod_range => 5,
        :mod_range_slide => 0,
        :mod_pulse_width => 0.5,
        :mod_pulse_width_slide => 0,
        :mod_phase_offset => 0,
        :mod_invert_wave => 0,
        :mod_wave => 1,
        :pulse_width => 0.5,
        :pulse_width_slide => 0
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
      "Emulation of the classic Roland TB-303 Bass Line synthesiser. Overdrive the res (i.e. use very small values) for that classic late 80s acid sound. "
    end

    def arg_defaults
      {
        :note => 52,
        :note_slide => 0,
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,

        :attack => 0,
        :decay => 0,
        :sustain => 0,
        :release => 1,
        :attack_level => 1,
        :sustain_level => 1,
        :env_curve => 2,

        :cutoff => 80,
        :cutoff_slide => 0,
        :cutoff_min => 30,
        :res => 0.1,
        :res_slide => 0,
        :wave => 0,
        :pulse_width => 0.5,
        :pulse_width_slide => 0
      }
    end

    def specific_arg_info
      {

        :cutoff =>
        {
          :doc => "",
          :validations => [v_positive(:cutoff), v_less_than(:cutoff, 130)],
          :modulatable => true
        },

        :cutoff_min =>
        {
          :doc => "",
          :validations => [v_positive(:cutoff), v_less_than(:cutoff_min, 130)],
          :modulatable => true
        },

        :wave =>
        {
          :doc => "Wave type - 0 saw, 1 pulse, 2 triangle",
          :validations => [v_one_of(:wave, [0, 1, 2])],
          :modulatable => true
        },

        :pulse_width =>
        {
          :doc => "Only valid if wave is type pulse.",
          :validations => [v_positive(:pulse_width)],
          :modulatable => true
        },

        :pulse_width_slide =>
        {
          :doc => "Time in seconds for pulse width to change. Only valid if wave is type pulse.",
          :validations => [v_positive(:pulse_width_slide)],
          :modulatable => true,
          :bpm_scale => true
        }

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
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,

        :attack => 0,
        :decay => 0,
        :sustain => 0,
        :release => 1,
        :attack_level => 1,
        :sustain_level => 1,
        :env_curve => 2,

        :cutoff => 130,
        :cutoff_slide => 0,
        :res => 0.3,
        :res_slide => 0

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
     "Saw wave with oscillating timbre. Produces moving saw waves with a unique character controllable with the control oscillator (usage similar to mod synths). "
    end

    def arg_defaults
      {
        :note => 52,
        :note_slide => 0,
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,

        :attack => 0,
        :decay => 0,
        :sustain => 0,
        :release => 1,
        :attack_level => 1,
        :sustain_level => 1,

        :cutoff => 100,
        :cutoff_slide => 0,
        :res => 0.1,
        :res_slide => 0,

        :phase => 1,
        :phase_slide => 0,
        :phase_offset => 0,

        :wave => 3,
        :invert_wave => 1,
        :range => 24,
        :range_slide => 0,
        :disable_wave => 0,
        :pulse_width => 0.5,
        :pulse_width_slide => 0

      }
    end

    def specific_arg_info
      {
        :phase =>
        {
          :doc => "Phase duration in seconds of timbre modulation.",
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
          :doc => "range of the assocatied sync saw in MIDI notes from the main note. Modifies timbre.",
          :validations => [v_between_inclusive(:phase_offset, 0, 90)],
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
          :doc => "Invert sync freq control waveform (i.e. flip it on the y axis). 0=normal wave, 1=inverted wave.",
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
      "Dark and swirly, this synth uses Pulse Width Modulation (PWM) to create a timbre which continually moves around. This effect is created using the pulse ugen which produces a variable width square wave. We then control the width of the pulses using a variety of LFOs - sin-osc and lf-tri in this case. We use a number of these LFO modulated pulse ugens with varying LFO type and rate (and phase in some cases to provide the LFO with a different starting point. We then mix all these pulses together to create a thick sound and then feed it through a resonant low pass filter (rlpf). For extra bass, one of the pulses is an octave lower (half the frequency) and its LFO has a little bit of randomisation thrown into its frequency component for that extra bit of variety."
end

    def arg_defaults
      {
        :note => 52,
        :note_slide => 0,
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,

        :attack => 0,
        :decay => 0,
        :sustain => 0,
        :release => 1,
        :attack_level => 1,
        :sustain_level => 1,
        :env_curve => 2,

        :cutoff => 110,
        :cutoff_slide => 0,
        :res => 0.3,
        :res_slide => 0
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
        :pan => 0,
        :pan_slide => 0,

        :attack => 0,
        :decay => 0,
        :sustain => 0,
        :release => 1,
        :attack_level => 1,
        :sustain_level => 1,
        :env_curve => 2,

        :cutoff => 110,
        :cutoff_slide => 0,
        :res => 1,
        :res_slide => 0
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
      "Generates noise which results from flipping random bits in a word.  The spectrum is emphasised towards lower frequencies. Useful for generating percussive sounds such as snares and hand claps. Also useful for simulating wind or sea effects."
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

  class StudioInfo < SonicPiSynth

  end

  class SoundIn < StudioInfo
    def name
      "Sound In"
    end

    def introduced
      Version.new(2,0,0)
    end

    def synth_name
      "sound_in"
    end

    def arg_defaults
      {
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,
        :input => 0
      }
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

    def arg_defaults
      {
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,
        :rate => 1,
        :rate_slide => 0
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

  class MonoPlayer < StudioInfo
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
        :pan => 0,
        :pan_slide => 0,

        :attack => 0,
        :decay => 0,
        :sustain => -1,
        :release => 0,

        :attack_level => 1,
        :sustain_level => 1,
        :env_curve => 2,

        :rate => 1,
        :start => 0,
        :finish => 1
      }
    end

    def specific_arg_info
      {

        :attack =>
        {
          :doc => "",
          :validations => [v_positive(:attack)],
          :modulatable => false
        },

        :sustain =>
        {
          :doc => "",
          :validations => [v_positive(:attack)],
          :modulatable => false
        },

        :release =>
        {
          :doc => "",
          :validations => [[lambda{|args| v = args[:release] ; (v == -1) || (v >= 0)}, "must either be a positive value or -1"]],
          :modulatable => false
        },

        :rate =>
        {
          :doc => "",
          :validations => [v_not_zero(:rate)],
          :modulatable => false
        },

        :start =>
        {
          :doc => "",
          :validations => [v_between_inclusive(:start, 0, 1)],
          :modulatable => false
        },

        :finish =>
        {
          :doc => "",
          :validations => [v_between_inclusive(:finish, 0, 1)],
          :modulatable => false
        },

      }
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
        :amp_slide => 0.1
      }
    end

  end

  class FXInfo < BaseInfo
    def prefix
      "sonic-pi-"
    end

    def default_arg_info
      super.merge({
                    :pre_amp =>
                    {
                      :doc => "Amplification applied to the input signal immediately before it is passed to the FX.",
                      :validations => [v_positive(:pre_amp)],
                      :modulatable => true,
                      :bpm_scale => true
                    },

                    :pre_amp_slide =>
                    {
                      :doc => generic_slide_doc(:pre_amp),
                      :validations => [v_positive(:pre_amp)],
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

    def doc
      "Make the incoming signal sound more spacious or distant as if it were played in a large room or cave. Signal may also be dampened by reducing the ampitude of the higher frequencies."
    end

    def arg_defaults
      {
        :amp => 1,
        :amp_slide => 0,
        :mix => 0.4,
        :mix_slide => 0,
        :pre_amp => 1,
        :pre_amp_slide => 0,

        :room => 0.6,
        :room_slide => 0,
        :damp => 0.5,
        :damp_slide => 0
      }
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

  class FXBitcrusher < FXInfo
    def name
      "Bitcrusher"
    end

    def introduced
      Version.new(2,0,0)
    end

    def synth_name
      "fx_bitcrusher"
    end

    def doc
      "Creates lo-fi output by decimating and deconstructing the incoming audio by lowering both the sample rate and bit depth. The default sample rate for CD audio is 44100, so use values less than that for lo-fi sound. Similarly, the default bit depth for CD audio is 16, so use values less than that for that crunchy chip-tune sound full of artefacts and bitty distortion."
    end

    def arg_defaults
      {
        :amp => 1,
        :amp_slide => 0,
        :mix => 1,
        :mix_slide => 0,
        :pre_amp => 1,
        :pre_amp_slide => 0,
        :sample_rate => 10000,
        :sample_rate_slide => 0,
        :bits => 8,
        :bits_slide => 0
      }
    end

    def specific_arg_info
      {
        :sample_rate =>
        {
          :doc => "The sample rate the audio will be resampled at.",
          :validations => [v_positive_not_zero(:sample_rate)],
          :modulatable => true
        },

        :bits =>
        {
          :doc => "The bit depth of the resampled audio.",
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
        :amp_slide => 0
      }
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
      "Standard echo with variable phase duration (time between echoes) and decay (length of echo fade out). If you wish to have a phase duration longer than 2s, you need to specifiy the longest phase duration you'd like with the arg max_phase. Be warned, echo FX with very long phases can consume a lot of memory and take longer to initialise."
    end

    def arg_defaults
      {
        :amp => 1,
        :amp_slide => 0,
        :mix => 1,
        :mix_slide => 0,
        :pre_amp => 1,
        :pre_amp_slide => 0,
        :phase => 0.25,
        :phase_slide => 0,
        :decay => 2,
        :decay_slide => 0,
        :max_phase => 2,
        :amp => 1,
        :amp_slide => 0
      }
    end

    def specific_arg_info
      {
        :max_phase =>
        {
          :doc => "The maximum phase duration in seconds.",
          :validations => [v_positive_not_zero(:max_phase)],
          :modulatable => false
        },

        :phase =>
        {
          :doc => "The time between echoes in seconds.",
          :validations => [v_positive_not_zero(:phase)],
          :modulatable => true,
          :bpm_scale => true

        },

        :phase_slide =>
        {
          :doc => "Slide time in seconds between phase values",
          :validations => [v_positive(:phase_slide)],
          :modulatable => true,
          :bpm_scale => true
        },

        :decay =>
        {
          :doc => "The time it takes for the echoes to fade away in seconds.",
          :validations => [v_positive_not_zero(:decay)],
          :modulatable => true,
          :bpm_scale => true
        },

        :decay_slide =>
        {
          :doc => "Slide time in seconds between decay times",
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

    def doc
      "Modulates the amplitude of the input signal with a specific control wave and phase duration. With the default pulse wave, slices the signal in and out, with the triangle wave, fades the signal in and out and with the saw wave, phases the signal in and then dramatically out. Control wave may be inverted with the arg invert_wave for more variety."
    end

    def arg_defaults
      {
        :amp => 1,
        :amp_slide => 0,
        :mix => 1,
        :mix_slide => 0,
        :pre_amp => 1,
        :pre_amp_slide => 0,
        :phase => 0.25,
        :phase_slide => 0,
        :amp_min => 0,
        :amp_min_slide => 0,
        :amp_max => 1,
        :amp_max_slide => 0,
        :pulse_width => 0.5,
        :pulse_width_slide => 0,
        :phase_offset => 0,
        :wave => 1,
        :invert_wave => 0
      }
    end

    def specific_arg_info
      {
        :phase =>
        {
          :doc => "The phase duration (in seconds) of the slices",
          :validations => [v_positive_not_zero(:phase)],
          :modulatable => true,
          :bpm_scale => true
        },

        :phase_slide =>
        {
          :doc => "Slide time in seconds between phase values",
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
          :doc => "Slide time in seconds between width values",
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
          :doc => "Invert control waveform (i.e. flip it on the y axis). 0=normal wave, 1=inverted wave.",
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

    def doc
      "Versatile wobble FX. Will repeatedly modulate a range of filters (rlpf, rhpf) between two cutoff values using a range of control wave forms (saw, pulse, tri, sine). You may alter the phase duration of the wobble, and the resonance of the filter. Combines well with the dsaw synth for crazy dub wobbles. Cutoff value is at cutoff_min at the start of phase"
    end

    def arg_defaults
      {
        :amp => 1,
        :amp_slide => 0,
        :mix => 1,
        :mix_slide => 0,
        :pre_amp => 1,
        :pre_amp_slide => 0,
        :phase => 0.5,
        :phase_slide => 0,
        :cutoff_min => 60,
        :cutoff_min_slide => 0,
        :cutoff_max => 120,
        :cutoff_max_slide => 0,
        :res => 0.2,
        :res_slide => 0,
        :phase_offset => 0,
        :wave => 0,
        :pulse_width => 0.5,
        :pulse_width_slide => 0,
        :filter => 0
      }
    end

    def specific_arg_info
      {

        :cutoff_min =>
        {
          :doc => "Minimum (MIDI) note filter will move to whilst wobbling. Choose a lower note for a higher range of movement. Full range of movement is the distance between cutoff_max and cutoff_min",
          :validations => [v_positive(:cutoff_min), v_less_than(:cutoff_min, 130)],
          :modulatable => true
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
          :doc => "Maximum (MIDI) note filter will move to whilst wobbling. Choose a higher note for a higher range of movement. Full range of movement is the distance between cutoff_max and cutoff_min",
          :validations => [v_positive(:cutoff_max), v_less_than(:cutoff_max, 130)],
          :modulatable => true
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
          :doc => "The phase duration (in seconds) for filter modulation cycles",
          :validations => [v_positive_not_zero(:phase)],
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
          :doc => "Time in seconds for pulse width to change. Only valid if wave is type pulse.",
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
          :doc => "Filter used for wobble effect. Use 0 for a resonant low pass filter or 1 for a rsonant high pass filter",
          :validations => [v_one_of(:filter, [0, 1])],
          :modulatable => true
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
      {
        :amp => 1,
        :amp_slide => 0,
        :mix => 1,
        :mix_slide => 0,
        :pre_amp => 1,
        :pre_amp_slide => 0,
        :phase => 4,
        :phase_slide => 0,
        :phase_offset => 0,
        :cutoff_min => 60,
        :cutoff_min_slide => 0,
        :cutoff_max => 120,
        :cutoff_max_slide => 0,
        :res => 0.2,
        :res_slide => 0
      }
    end

    def specific_arg_info
      {
        :phase =>
        {
          :doc => "The phase duration (in seconds) for filter modulation cycles",
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
          :doc => "Minimum (MIDI) note filter will move to whilst wobbling. Choose a lower note for a higher range of movement. Full range of movement is the distance between cutoff_max and cutoff_min",
          :validations => [v_positive(:cutoff_min), v_less_than(:cutoff_min, 130)],
          :modulatable => true
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
          :doc => "Maximum (MIDI) note filter will move to whilst wobbling. Choose a higher note for a higher range of movement. Full range of movement is the distance between cutoff_max and cutoff_min",
          :validations => [v_positive(:cutoff_max), v_less_than(:cutoff_max, 130)],
          :modulatable => true
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
          :doc => "Filter used for wobble effect. Use 0 for a resonant low pass filter or 1 for a rsonant high pass filter",
          :validations => [v_one_of(:filter, [0, 1])],
          :modulatable => true
        }

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
      "Compresses the dynamic range of the incoming signal. Equivalent to automatically turning the amp down when the signal gets too loud and then back up again when it's quite. Useful for ensuring the containing signal doesn't overwhelm other aspects of the sound. Also a general purpose hard-knee dynamic range processor which can be tuned via the arguments to both expand and compress the signal."
    end

    def arg_defaults
      {
        :amp => 1,
        :amp_slide => 0,
        :mix => 1,
        :pre_amp => 1,
        :pre_amp_slide => 0,
        :threshold => 0.2,
        :threshold_slide => 0,
        :clamp_time => 0.01,
        :clamp_time_slide => 0,
        :slope_above => 0.5,
        :slope_above_slide => 0,
        :slope_below => 1,
        :slope_below_slide => 0,
        :relax_time => 0.01,
        :relax_time_slide => 0
      }
    end

    def specific_arg_info
      {

        :threshold =>
        {
          :doc => "threshold value determining the break point between slope_below and slope_above. ",
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
          :doc => "Slope of the amplitude curve below the threshold. A value of 1 means that the output of signals with amplitude below the threshold will be unaffected. Greater values will magnify and smaller values will attenuate the signal.",
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
          :doc => "Time taken for the amplitude adjustments to be released. Usually a little longer than clamp_time. If both times are too short, you can get some (possibly unwanted) artifacts. Also known as the time of the release phase.",
          :validations => [v_positive(:clamp_time)],
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


  class FXRLPF < FXInfo
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
      {
        :amp => 1,
        :amp_slide => 0,
        :mix => 1,
        :mix_slide => 0,
        :pre_amp => 1,
        :pre_amp_slide => 0,
        :cutoff => 100,
        :cutoff_slide => 0,
        :res => 0.5,
        :res_slide => 0
      }
    end

    def specific_arg_info
      {


      }
    end

    def doc
      "Dampens the parts of the signal that are above than the cutoff point (typically the crunchy fizzy harmonic overtones) and keeps the lower parts (typicaly the bass/mid of the sound). behaviour, The resonant part of the resonant low pass filter emphasises/resonates the frequencies around the cutoff point. The amount of emphasis is controlled by the res param with a lower res resulting in greater resonance. High amounts of resonance (rq ~0) can create a whistling sound around the cutoff frequency.

Choose a higher cutoff to keep more of the high frequences/treble of the sound and a lower cutoff to make the sound more dull and only keep the bass."
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

  class FXRHPF < FXInfo
    def name
      "Resonant High Pass Filter"
    end

    def doc
      "Dampens the parts of the signal that are lower than the cutoff point (typicaly the bass of the sound) and keeps the higher parts (typically the crunchy fizzy harmonic overtones). The resonant part of the resonant low pass filter emphasises/resonates the frequencies around the cutoff point. The amount of emphasis is controlled by the res param with a lower res resulting in greater resonance. High amounts of resonance (rq ~0) can create a whistling sound around the cutoff frequency.

Choose a lower cutoff to keep more of the bass/mid and a higher cutoff to make the sound more light and crispy. "
    end

    def introduced
      Version.new(2,0,0)
    end

    def synth_name
      "fx_rhpf"
    end

    def arg_defaults
      {
        :amp => 1,
        :amp_slide => 0,
        :mix => 1,
        :mix_slide => 0,
        :pre_amp => 1,
        :pre_amp_slide => 0,
        :cutoff => 100,
        :cutoff_slide => 0,
        :res => 0.5,
        :res_slide => 0
      }
    end

    def specific_arg_info
      {


      }
    end
  end

  class FXNormRHPF < FXRLPF
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
      "Dampens the parts of the signal that are above than the cutoff point(typically the crunchy fizzy harmonic overtones) and keeps the lower parts (typicaly the bass/mid of the sound). Choose a higher cutoff to keep more of the high frequences/treble of the sound and a lower cutoff to make the sound more dull and only keep the bass."
    end

    def arg_defaults
      {
        :amp => 1,
        :amp_slide => 0,
        :mix => 1,
        :mix_slide => 0,
        :pre_amp => 1,
        :pre_amp_slide => 0,
        :cutoff => 100,
        :cutoff_slide => 0
      }
    end

    def specific_arg_info
      {


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
      "Dampens the parts of the signal that are lower than the cutoff point (typicaly the bass of the sound) and keeps the higher parts (typically the crunchy fizzy harmonic overtones). Choose a lower cutoff to keep more of the bass/mid and a higher cutoff to make the sound more light and crispy. "
    end

    def arg_defaults
      {
        :amp => 1,
        :amp_slide => 0,
        :mix => 1,
        :mix_slide => 0,
        :pre_amp => 1,
        :pre_amp_slide => 0,
        :cutoff => 100,
        :cutoff_slide => 0
      }
    end
  end

  class FXNormHPF < FXRLPF
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
      {
        :amp => 1,
        :amp_slide => 0,
        :mix => 1,
        :mix_slide => 0,
        :pre_amp => 1,
        :pre_amp_slide => 0,
        :level => 1,
        :level_slide => 0,
        :amp => 1,
        :amp_slide => 0
      }
    end

    def specific_arg_info
      {
        :level =>
        {
          :doc => "The peak output amplitude level to which to normalise the in",
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
      {
        :amp => 1,
        :amp_slide => 0,
        :mix => 1,
        :mix_slide => 0,
        :pre_amp => 1,
        :pre_amp_slide => 0,
        :distort => 0.5,
        :distort_slide => 0
      }
    end

    def specific_arg_info
      {
        :distort =>
        {
          :doc => "Amount of distortion to be applied (as a value between 0 ad 1)",
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
      {
        :amp => 1,
        :amp_slide => 0,
        :mix => 1,
        :mix_slide => 0,
        :pre_amp => 1,
        :pre_amp_slide => 0,
        :pan => 0,
        :pan_slide => 0
      }
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
          :drum_bass_hard]},

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
          :guit_e_slide]},

      :misc => {
        :desc => "Miscellaneous Sounds",
        :prefix => "misc_",
        :samples => [
          :misc_burp]},

      :perc => {
        :desc => "Percussive Sounds",
        :prefix => "perc_",
        :samples => [
          :perc_bell]},

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
          :ambi_choir]},

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

      :loop => {
        :desc => "Sounds for Looping",
        :prefix => "loop_",
        :samples => [
          :loop_industrial,
          :loop_compus,
          :loop_amen,
          :loop_amen_full]}}

    @@all_samples = (@@grouped_samples.values.reduce([]) {|s, el| s << el[:samples]}).flatten

    @@synth_infos =
      {
      :dull_bell => DullBell.new,
      :pretty_bell => PrettyBell.new,
      :beep => Beep.new,
      :saw => Saw.new,
      :pulse => Pulse.new,
      :tri => Tri.new,
      :dsaw => DSaw.new,
      :fm => FM.new,
      :mod_fm => ModFM.new,
      :mod_saw => ModSaw.new,
      :mod_dsaw => ModDSaw.new,
      :mod_sine => ModSine.new,
      :mod_tri => ModTri.new,
      :mod_pulse => ModPulse.new,
      :tb303 => TB303.new,
      :supersaw => Supersaw.new,
      :prophet => Prophet.new,
      :zawa => Zawa.new,
      :mono_player => MonoPlayer.new,
      :stereo_player => StereoPlayer.new,

      :sound_in => SoundIn.new,
      :noise => Noise.new,
      :pnoise => PNoise.new,
      :bnoise => BNoise.new,
      :gnoise => GNoise.new,
      :cnoise => CNoise.new,

      :basic_mono_player => BasicMonoPlayer.new,
      :basic_stereo_player => BasicStereoPlayer.new,
      :basic_mixer => BasicMixer.new,

#      :fx_bitcrusher => FXBitcrusher.new,
      :fx_reverb => FXReverb.new,
      :fx_replace_reverb => FXReverb.new,
      :fx_level => FXLevel.new,
      :fx_replace_level => FXLevel.new,
      :fx_echo => FXEcho.new,
      :fx_replace_echo => FXEcho.new,
      :fx_slicer => FXSlicer.new,
      :fx_replace_slicer => FXSlicer.new,
      :fx_wobble => FXWobble.new,
      :fx_replace_wobble => FXWobble.new,
      :fx_ixi_techno => FXIXITechno.new,
      :fx_replace_ixi_techno => FXIXITechno.new,
      :fx_compressor => FXCompressor.new,
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
      :fx_replace_pan => FXPan.new

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

    def self.info_doc_html_map(klass)
      key_mod = nil
      res = {}
      hv_face = "face=\"HelveticaNeue-Light,Helvetica Neue Light,Helvetica Neue\""

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
        doc = ""
        doc << '<p> <span style="font-size:25px; color:white;background-color:deeppink;">'
        doc << "<font #{hv_face}>" << v.name << "</font></span></p>\n"
        if klass == SynthInfo
          safe_k = k
          doc << "<h2><font color=\"#3C3C3C\"><pre>use_synth"
          doc << " <font color=\"DeepPink\">:#{safe_k}</font></pre></h2>\n"

        else
          safe_k = k.to_s[3..-1]
          doc << "<h2><pre><font color=\"#3C3C3C\">with_fx"
          doc << " <font color=\"DeepPink\">:#{safe_k}</font> <font color=\"DarkOrange\">do</font><br/>  play <font color=\"DodgerBlue\">50</font><br/><font color=\"DarkOrange\">end</font></pre></font></h2>\n"
        end

        cnt = 0
        doc << "<table cellpadding=\"2\">\n <tr>"
        arglist = ""
        v.arg_info.each do |ak, av|
          arglist << "</tr><tr>" if cnt%6 == 0
          bg_colour = cnt.even? ? "#5e5e5e" : "#E8E8E8"
          fnt_colour = cnt.even? ? "white" : "#5e5e5e"
          cnt += 1
          arglist << "<td bgcolor=\"#{bg_colour}\">\n  <pre><h4><font color=\"#{fnt_colour}\">#{ak}: </font></h4</pre>\n</td>\n<td bgcolor=\"#{bg_colour}\">\n  <pre><h4><font color=\"#{fnt_colour}\">#{av[:default]}</font></h4></pre>\n</td>\n"
        end
        arglist << "</tr></table>\n"
        doc << arglist


        doc << "<p><font size=\"4\", #{hv_face}>"
        doc << "  " << v.doc << "</font></p>\n"

        doc << "<p><font size=\"3\", #{hv_face}>\n"
        doc << "<span style=\"color:white;background-color:darkorange;\">"
        doc << "Introduced in v" << v.introduced.to_s << "\n</span></p>\n"

        doc << "<table cellpadding=\"8\">\n"
        doc << "<tr><th></th><th></th></tr>\n"

        cnt = 0
        v.arg_info.each do |ak, av|
          cnt += 1
          background_colour = cnt.even? ? "#F8F8F8" : "#E8E8E8"
          key_bg_colour = cnt.even? ? "#E6F0FF" : "#B2D1FF"
          doc << "  <tr bgcolor=\"#{background_colour}\">\n"
          doc << "    <td bgcolor=\"#{key_bg_colour}\"><h3><pre> #{ak}:</pre></h3></td>\n"
          doc << "      <td>\n"
          doc << "        <font size=\"4\", #{hv_face}>\n"
          doc << "          #{av[:doc] || 'write me'}<br/></font>\n"
          doc << "          <em><font size=\"3\", #{hv_face}>Default: #{av[:default]}<br/>\n"
          doc << "          #{av[:constraints].join(",")}<br/>\n" unless av[:constraints].empty?
          doc << "          #{av[:modulatable] ? "May be changed whilst playing" : "Can not be changed once set"}\n"
          doc << "       </font></em>\n"
          doc << "     </td>\n"
          doc << " </tr>\n"
        end
        doc << "  </table>\n"
        res["#{safe_k}"] = doc
      end
      res
    end

    def self.info_doc_markdown(name, klass, key_mod=nil)
      res = "# #{name}\n\n"

      get_all.each do |k, v|
        next unless v.is_a? klass
        snake_case = v.name.downcase.gsub(/ /, "-")
        res << "* [#{v.name}](##{snake_case})\n"
      end
      res << "\n"
      get_all.each do |k, v|
        next unless v.is_a? klass
        res << "## " << v.name << "\n\n"
        res << "### Key:\n"
        mk = key_mod ? key_mod.call(k) : k
        res << "  :#{mk}\n\n"
        res << "### Doc:\n"
        res << "  " << v.doc << "\n\n"
        res << "### Arguments:" "\n"
        v.arg_info.each do |ak, av|
          res << "  * #{ak}:\n"
          res << "    - doc: #{av[:doc] || 'write me'}\n"
          res << "    - default: #{av[:default]}\n"
          res << "    - constraints: #{av[:constraints].empty? ? "none" : av[:constraints].join(",")}\n"
          res << "    - #{av[:modulatable] ? "May be changed whilst playing" : "Can not be changed once set"}\n\n"
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
      hv_face = "face=\"HelveticaNeue-Light,Helvetica Neue Light,Helvetica Neue\""
      res = {}

      grouped_samples.each do |k, v|
      cnt = 0
        cnt = 0
        html = ""
        html << '<p> <span style="font-size:25px; color:white;background-color:deeppink;">'
        html << "<font #{hv_face}>" << v[:desc] << "</font></span></p>\n"
        html << "<table cellpadding=\"2\">\n <tr>"
        arglist = ""
        StereoPlayer.new.arg_info.each do |ak, av|
          arglist << "</tr><tr>" if cnt%6 == 0
          bg_colour = cnt.even? ? "#5e5e5e" : "#E8E8E8"
          fnt_colour = cnt.even? ? "white" : "#5e5e5e"
          cnt += 1
          arglist << "<td bgcolor=\"#{bg_colour}\">\n  <pre><h4><font color=\"#{fnt_colour}\">#{ak}: </font></h4</pre>\n</td>\n<td bgcolor=\"#{bg_colour}\">\n  <pre><h4><font color=\"#{fnt_colour}\">#{av[:default]}</font></h4></pre>\n</td>\n"
        end
        arglist << "</tr></table>\n"
        html << arglist

        html << "<table cellpadding=\"2\">\n <tr>"

        v[:samples].each do |s|
          html << "  <tr><td bgcolor=\"white\"><h2><pre><font color=\"#3C3C3C\"> sample</font> <font color=\"DeepPink\">:#{s}<font></pre></h2></td></tr>\n"
        end
        html << "</table>\n"
        doc = ""
        doc << "<table cellpadding=\"10\">\n"
        doc << "<tr><th></th><th></th></tr>\n"

        cnt = 0
        StereoPlayer.new.arg_info.each do |ak, av|
          cnt += 1
          background_colour = cnt.even? ? "#F8F8F8" : "#E8E8E8"
          key_bg_colour = cnt.even? ? "#E6F0FF" : "#B2D1FF"
          doc << "  <tr bgcolor=\"#{background_colour}\">\n"
          doc << "    <td bgcolor=\"#{key_bg_colour}\"><h3><pre> #{ak}:</pre></h3></td>\n"
          doc << "      <td>\n"
          doc << "        <font size=\"4\", #{hv_face}>\n"
          doc << "          #{av[:doc] || 'write me'}<br/></font>\n"
          doc << "          <font size=\"3\", #{hv_face}>Default: #{av[:default]}<br/>\n"
          doc << "          #{av[:constraints].join(",")}<br/>\n" unless av[:constraints].empty?
          doc << "          #{av[:modulatable] ? "May be changed whilst playing" : "Can not be changed once set"}\n"
          doc << "       </font>\n"
          doc << "     </td>\n"
          doc << " </tr>\n"
        end
        doc << "  </table>\n"
        html << doc
        res[v[:desc]] = html
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
  end
end
