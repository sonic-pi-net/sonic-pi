module SonicPi
  class SynthInfo
    def doc
      raise "please implement me!"
    end

    def arg_defaults
      raise "please implement me!"
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

    def validate!(args)
      args = Hash[*args] if args.is_a? Array
      info = arg_info
      args.keys.each do |a|
        a_sym = a.to_sym
        validations = arg_validations(a)
        validations.each do |vali|
          raise "Value of argument #{a_sym.inspect}  must a number, got #{args[a_sym].inspect}." unless args[a_sym].is_a? Numeric
          v, m = vali
          raise "Value of argument #{a_sym.inspect} #{m}, got #{args[a_sym].inspect}." unless v.call(args)
        end
      end
    end

    def arg_validations(arg_name)
      info = default_arg_info.merge(specific_arg_info)
      arg_info = info[arg_name] || {}
      arg_info[:validations] || []
    end

    def arg_info
      info = default_arg_info.merge(specific_arg_info)
      res = {}
      arg_defaults.each do |arg, default|
        default_info = info[arg] || {}
        constraints = (default_info[:validations] || []).map{|el| el[1]}
        new_info = {}
        new_info[:doc] = default_info[:doc]
        new_info[:default] = default
        new_info[:constraints] = constraints
        res[arg] = new_info
      end

      res

    end

    private

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

    def v_one_of(arg, valid_options)
      [lambda{|args| valid_options.include?(args[arg])}, "must be one of the following values: #{valid_options.inspect}"]
    end

    def default_arg_info
      {
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
          :modulatable => true
        },

        :amp =>
        {
          :doc => "The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played.",
          :validations => [v_positive(:amp)],
          :modulatable => true
        },

        :amp_slide =>
        {
          :doc => "Amount of time (in seconds) for the amp to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.",
          :validations => [v_positive(:amp_slide)],
          :modulatable => true
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
          :modulatable => true
        },


        :attack =>
        {
          :doc => "Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.",
          :validations => [v_positive(:attack)],
          :modulatable => false
        },


        :sustain =>
        {
          :doc => "Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.",
          :validations => [v_positive(:sustain)],
          :modulatable => false
        },


        :release =>
        {
          :doc => "Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.",
          :validations => [v_positive(:release)],
          :modulatable => false
        },

        :cutoff =>
        {
          :doc => "MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.",
          :validations => [v_positive(:cutoff), v_less_than(:cutoff, 130)],
          :modulatable => true
        },

        :cutoff_slide =>
        {
          :doc => "Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value.",
          :validations => [v_positive(:cutoff_slide)],
          :modulatable => true
        },

        :detune =>
        {
          :doc => "Distance (in MIDI notes) between components of sound. Affects thickness, sense of tuning and harmony. Tiny values such as 0.1 create a thick sound. Larger values such as 0.5 make the tuning sound strange. Even bigger values such as 5 create chord-like sounds.",
          :validations => [],
          :modulatable => true
        },

        :detune_slide =>
        {
          :doc => "Amount of time (in seconds) for the detune value to change. A long detune_slide value means that the detune takes a long time to slide from the previous value to the new value. A detune_slide of 0 means that the detune instantly changes to the new value.",
          :validations => [v_positive(:detune_slide)],
          :modulatable => true
        }


      }
    end

    def specific_arg_info
      {}
    end

  end

  class DullBell < SynthInfo
    def name
      "Dull Bell"
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
        :attack => 0.01,
        :sustain => 0,
        :release => 1
      }
    end
  end

  class PrettyBell < DullBell
    def name
      "Pretty Bell"
    end

    def doc
      "A simple pretty bell sound."
    end
  end

  class Beep < SynthInfo
    def name
      "Sine Wave"
    end

    def doc
      "A simple pure sine wave."
    end

    def arg_defaults
      {
        :note => 52,
        :note_slide => 0,
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,
        :attack => 0.1,
        :sustain => 0,
        :release => 0.3
      }
    end
  end

  class SawBeep < Beep
    def name
      "Saw Wave"
    end

    def doc
      "A simple saw wave with a low pass filter."
    end
  end

  class DSaw < SynthInfo
    def name
      "Detuned Saw wave"
    end

    def doc
      "A pair of detuned saw waves with a lop pass filter."
    end

    def arg_defaults
      {
        :note => 52,
        :note_slide => 0,
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,

        :attack => 0.1,
        :sustain => 0,
        :release => 0.3,

        :cutoff => 100,
        :cutoff_slide => 0,
        :detune => 0.1,
        :detune_slide => 0
      }
    end
  end

  class FM < SynthInfo
    def name
      "Basic FM synthesis"
    end

    def doc
      ""
    end

    def arg_defaults
      {
        :note => 52,
        :note_slide => 0,
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,

        :attack => 1,
        :sustain => 0,
        :release => 1,

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
          :doc => "",
          :validations => [],
          :modulatable => true
        },

        :divisor_slide =>
        {
          :doc => "",
          :validations => [v_positive(:divisor_slide)],
          :modulatable => true
        },

        :depth =>
        {
          :doc => "",
          :validations => [],
          :modulatable => true
        },

        :depth_slide =>
        {
          :doc => "",
          :validations => [v_positive(:depth_slide)],
          :modulatable => true
        }
      }

    end
  end

  class ModSaw < SynthInfo
    def name
      "Modulated Saw Wave"
    end

    def doc
      ""
    end

    def arg_defaults
      {
        :note => 52,
        :note_slide => 0,
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,

        :attack => 0.01,
        :sustain => 0,
        :release => 2,

        :cutoff => 100,
        :cutoff_slide => 0,
        :mod_rate => 1,
        :mod_rate_slide => 0,
        :mod_range => 5,
        :mod_range__slide => 0,
        :mod_width => 0.5,
        :mod_width_slide => 0
      }
    end
  end

  class ModSawS < SynthInfo
    def name
      "Modulated Saw Wave Simple"
    end

    def doc
      ""
    end

    def arg_defaults
      {
        :note => 52,
        :amp => 1,
        :pan => 0,
        :attack => 0.01,
        :sustain => 0,
        :release => 2,
        :slide => 0,

        :mod_rate => 1,
        :mod_range => 5,
        :mod_width => 0.5
      }
    end
  end

  class ModDSaw < SynthInfo
    def name
      "Modulated Detuned Saw Waves"
    end

    def doc
      ""
    end

    def arg_defaults
      {
        :note => 52,
        :amp => 1,
        :pan => 0,
        :attack => 0.01,
        :sustain => 0,
        :release => 2,
        :slide => 0,

        :cutoff => 100,
        :cutoff_slide => 0,
        :mod_rate => 1,
        :mod_range => 5,
        :mod_width => 0.5,
        :detune => 0.1
      }
    end
  end

  class ModDSawS < SynthInfo
    def name
      "Modulated Detuned Saw Waves Simple"
    end

    def doc
      ""
    end

    def arg_defaults
      {
        :note => 52,
        :note_slide => 0,
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,

        :attack => 0.01,
        :sustain => 0,
        :release => 2,

        :mod_rate => 1,
        :mod_rate_slide => 0,
        :mod_range => 5,
        :mod_range_slide => 0,
        :mod_width => 0.5,
        :mod_width_slide => 0,
        :detune => 0.1,
        :detune_slide => 0
      }
    end
  end

  class ModSine < SynthInfo
    def name
      "Modualted Sine Wave"
    end

    def doc
      ""
    end

    def arg_defaults
      {
        :note => 52,
        :note_slide => 0,
        :amp => 1,
        :note_slide => 0,
        :pan => 0,
        :note_slide => 0,
        :attack => 0.01,
        :sustain => 0,
        :release => 2,

        :cutoff => 100,
        :cutoff_slide => 0,
        :mod_rate => 1,
        :mod_rate_slide => 0,
        :mod_range => 5,
        :mod_range_slide => 0,
        :mod_width => 0.5,
        :mod_width_slide => 0

      }
    end
  end

  class ModSineS < SynthInfo
    def name
      "Modualted Sine Wave Simple"
    end

    def doc
      ""
    end

    def arg_defaults
      {
        :note => 52,
        :note_slide => 0,
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,
        :attack => 0.01,
        :sustain => 0,
        :release => 2,
        :slide => 0,

        :mod_rate => 1,
        :mod_rate_slide => 0,
        :mod_range => 5,
        :mod_range_slide => 0,
        :mod_width => 0.5,
        :mod_width_slide => 0
      }
    end
  end

  class ModTri < SynthInfo
    def name
      "Modulated Triangle Wave"
    end

    def doc
      ""
    end

    def arg_defaults
      {
        :note => 52,
        :note_slide => 0,
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :amp_slide => 0,
        :attack => 0.01,
        :sustain => 0,
        :release => 2,

        :cutoff => 100,
        :cutoff_slide => 0,
        :mod_rate => 1,
        :mod_rate_slide => 0,
        :mod_range => 5,
        :mod_range_slide => 0,
        :mod_width => 0.5,
        :mod_width_slide => 0
      }
    end
  end

  class ModTriS < SynthInfo
    def name
      "Modulated Triangle Wave Simple"
    end

    def doc
      ""
    end


    def arg_defaults
      {
        :note => 52,
        :note_slide => 0,
        :amp => 1,
        :note_slide => 0,
        :pan => 0,
        :note_slide => 0,
        :attack => 0.01,
        :sustain => 0,
        :release => 2,
        :slide => 0,

        :mod_rate => 1,
        :note_slide => 0,
        :mod_range => 5,
        :note_slide => 0,
        :mod_width => 0.5,
        :note_slide => 0
      }
    end
  end

  class ModPulse < SynthInfo
    def name
      "Modulated Pulse"
    end

    def doc
      ""
    end

    def arg_defaults
      {
        :note => 52,
        :note_slide => 0,
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,
        :attack => 0.01,
        :sustain => 0,
        :release => 2,

        :cutoff => 100,
        :cutoff_slide => 0,
        :mod_rate => 1,
        :mod_rate__slide => 0,
        :mod_range => 5,
        :mod_range_slide => 0,
        :mod_width => 0.5,
        :mod_width_slide => 0,
        :pulse_width => 0.5,
        :pulse_width_slide => 0
      }
    end
  end

  class ModPulseS < SynthInfo
    def name
      "Modulated Pulse Simple"
    end

    def doc
      ""
    end

    def arg_defaults
      {
        :note => 52,
        :note_slide => 0,
        :amp => 1,
        :note_slide => 0,
        :pan => 0,
        :note_slide => 0,
        :attack => 0.01,
        :sustain => 0,
        :release => 2,

        :mod_rate => 1,
        :note_slide => 0,
        :mod_range => 5,
        :note_slide => 0,
        :mod_width => 0.5,
        :note_slide => 0,
        :pulse_width => 0.5,
        :note_slide => 0
      }
    end
  end

  class TB303 < SynthInfo
    def name
      "tb-303"
    end

    def doc
      ""
    end

    def arg_defaults
      {
        :note => 52,
        :note_slide => 0,
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,
        :attack => 0.01,
        :sustain => 0,
        :release => 2,

        :cutoff => 80,
        :cutoff_slide => 0,
        :cutoff_min => 30,
        :res => 0.2,
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
          :validations => [v_positive(:cutoff), v_less_than(:cutoff, 130)],
          :modulatable => true
        },

        :wave =>
        {
          :doc => "Wave type - 0 saw, 1 pulse",
          :validations => [v_one_of(:wave, [0, 1])],
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
          :modulatable => true
        }

      }
    end
  end

  class Supersaw < SynthInfo
    def doc
      ""
    end

    def arg_defaults
      {
        :note => 52,
        :note_slide => 0,
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,
        :attack => 0.01,
        :sustain => 0,
        :release => 2,

        :cutoff => 130,
        :cutoff_slide => 0,
        :res => 0.3,
        :res_slide => 0

      }
    end
  end

  class SupersawS < SynthInfo
    def name
      "Supersaw Simple"
    end

    def doc
      ""
    end

    def arg_defaults
      {
        :note => 52,
        :note_slide => 0,
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,
        :attack => 0.01,
        :sustain => 0,


      }
    end

  end


  class Prophet < SynthInfo
    def name
      "The Prophet"
    end

    def doc
      "Dark and swirly, this synth uses Pulse Width Modulation
      (PWM) to create a timbre which continually moves around. This
      effect is created using the pulse ugen which produces a variable
      width square wave. We then control the width of the pulses using a
      variety of LFOs - sin-osc and lf-tri in this case. We use a number
      of these LFO modulated pulse ugens with varying LFO type and rate
      (and phase in some cases to provide the LFO with a different
      starting point. We then mix all these pulses together to create a
      thick sound and then feed it through a resonant low pass filter
      (rlpf).

      For extra bass, one of the pulses is an octave lower (half the
      frequency) and its LFO has a little bit of randomisation thrown
      into its frequency component for that extra bit of variety."
    end

    def arg_defaults
      {
        :note => 52,
        :note_slide => 0,
        :amp => 1,
        :amp_slide => 0,
        :pan => 0,
        :pan_slide => 0,
        :attack => 0.01,
        :sustain => 0,
        :release => 2,

        :cutoff => 110,
        :cutoff_slide => 0,
        :res => 0.3,
        :res_slide => 0
      }
    end

  end

  class BasicMonoPlayer < SynthInfo
    def name
      "Basic Mono Sample Player - (no envelope)"
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
      "Basic Stereo Sample Player - (no envelope)"
    end

    def doc
      ""
    end
  end

  class MonoPlayer < SynthInfo
    def name
      "Mono Sample Player"
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
        :sustain => -1,
        :release => 0,
        :rate => 1,
        :start => 0,
        :end => 1
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
          :validations => [],
          :modulatable => false
        },

        :start =>
        {
          :doc => "",
          :validations => [v_positive(:start), v_between_inclusive(:start, 0, 1)],
          :modulatable => false
        },

        :end =>
        {
          :doc => "",
          :validations => [v_positive(:end), v_between_inclusive(:end, 0, 1)],
          :modulatable => false
        },

      }
    end

  end

  class StereoPlayer < MonoPlayer
    def name
      "Stereo Sample Player"
    end
  end

  class FXReverb < SynthInfo
    def name
      "FX Reverb"
    end

    def arg_defaults
      {
        :mix => 0.75,
        :mix_slide => 0,
        :room => 0.6,
        :room_slide => 0,
        :damp => 0.5,
        :damp_slide => 0
      }
    end
  end

  class FXLevel < SynthInfo
    def name
      "FX Level Amplifier"
    end

    def arg_defaults
      {
        :amp => 1,
        :amp_slide => 0
      }
    end
  end

  class FXEcho < SynthInfo
    def name
      "FX Echo"
    end

    def arg_defaults
      {
        :max_delay => 1,
        :delay => 0.4,
        :delay_slide => 0,
        :decay => 8,
        :decay_slide => 0,
        :amp => 1
      }
    end

    def specific_arg_info
      {
        :max_delay =>
        {
          :doc => "The maximum delay time in seconds.",
          :validations => [v_positive_not_zero(:max_delay)],
          :modulatable => false
        },

        :delay =>
        {
          :doc => "The time between echoes in seconds.",
          :validations => [v_positive_not_zero(:delay)],
          :modulatable => true
        },

        :delay_slide =>
        {
          :doc => "Slide time in seconds between delay values",
          :validations => [v_positive(:delay_slide)],
          :modulatable => true
        },

        :decay =>
        {
          :doc => "The time it takes for the echoes to fade away in seconds.",
          :validations => [v_positive_not_zero(:decay)],
          :modulatable => true
        },

        :decay_slide =>
        {
          :doc => "Slide time in seconds between decay times",
          :validations => [v_positive(:decay_slide)],
          :modulatable => true
        }
      }
    end
  end

  class FXSlicer < SynthInfo
    def name
      "FX Slicer"
    end

    def arg_defaults
      {
        :rate => 4,
        :rate_slide => 0,
        :width => 0.5,
        :width_slide => 0,
        :phase => 0,
        :amp => 1,
        :amp_slide => 0.05
      }
    end

    def specific_arg_info
      {
        :rate =>
        {
          :doc => "The frequency of the slices",
          :validations => [v_positive_not_zero(:rate)],
          :modulatable => true
        },

        :rate_slide =>
        {
          :doc => "Slide time in seconds between rate values",
          :validations => [v_positive(:rate_slide)],
          :modulatable => true
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
          :modulatable => true
        },

        :phase =>
        {
          :doc => "Initial phase.",
          :validations => [v_between_inclusive(:phase, 0, 1)],
          :modulatable => false
        },

        :amp_slide =>
        {
          :doc => "The slide lag time for amplitude changes.",
          :validations => [v_positive(:amp_slide)],
          :modulatable => true
        },

        :amp =>
        {
          :doc => "The amplitude of the resulting effect.",
          :validations => [v_positive(:amp)],
          :modulatable => true
        }
      }
    end
  end


  class FXTechno < SynthInfo
    def name
      "FX Techno"
    end

    def arg_defaults
      {
        :rate => 0.1,
        :rate_slide => 0,
        :cutoff_min => 880,
        :cutoff_min_slide => 0,
        :cutoff_max => 12000,
        :cutoff_max_slide => 0,
        :res => 0.2,
        :res_slide => 0
      }
    end

    def specific_arg_info
      {
        :rate =>
        {
          :doc => "The frequency of filter modulation",
          :validations => [v_positive_not_zero(:rate)],
          :modulatable => true
        }

      }
    end
  end


  class FXCompressor < SynthInfo
    def name
      "FX Compressor"
    end

    def arg_defaults
      {
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


      }
    end
  end


  class FXRLPF < SynthInfo
    def name
      "FX Resonant Low Pass Filter"
    end

    def arg_defaults
      {
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

  class FXNormRLPF < FXRLPF
    def name
      "FX Normalised Resonant Low Pass Filter"
    end
  end

  class FXRHPF < SynthInfo
    def name
      "FX Resonant High Pass Filter"
    end

    def arg_defaults
      {
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
      "FX Normalised Resonant High Pass Filter"
    end
  end

  class FXLPF < SynthInfo
    def name
      "FX Low Pass Filter"
    end

    def arg_defaults
      {
        :cutoff => 100,
        :cutoff_slide => 0
      }
    end

    def specific_arg_info
      {


      }
    end
  end

  class FXNormLPF < FXRLPF
    def name
      "FX Normalised Low Pass Filter"
    end
  end

  class FXHPF < SynthInfo
    def name
      "FX High Pass Filter"
    end

    def arg_defaults
      {
        :cutoff => 100,
        :cutoff_slide => 0
      }
    end

    def specific_arg_info
      {


      }
    end
  end

  class FXNormHPF < FXRLPF
    def name
      "FX Normalised High Pass Filter"
    end
  end

  class FXNormaliser < SynthInfo
    def name
      "FX Normaliser"
    end

    def arg_defaults
      {
        :amp => 1,
        :amp_slide => 0
      }
    end
  end



  class SynthInfo
    @@synth_infos =
      {
      :dull_bell => DullBell.new,
      :pretty_bell => PrettyBell.new,
      :saw_beep => SawBeep.new,
      :dsaw => DSaw.new,
      :fm => FM.new,
      :mod_saw => ModSaw.new,
      :mod_saw_s => ModSawS.new,
      :mod_dsaw => ModDSaw.new,
      :mod_dsaw_s => ModDSawS.new,
      :mod_sine => ModSine.new,
      :mod_sine_s => ModSineS.new,
      :mod_tri => ModTri.new,
      :mod_tri_s => ModTriS.new,
      :mod_pulse => ModPulse.new,
      :mod_pulse_s => ModPulseS.new,
      :tb303 => TB303.new,
      :supersaw => Supersaw.new,
      :supersaw_s => SupersawS.new,
      :prophet => Prophet.new,
      :mono_player => MonoPlayer.new,
      :stereo_player => StereoPlayer.new,
      :basic_mono_player => BasicMonoPlayer.new,
      :basic_stereo_player => BasicStereoPlayer.new,

      :fx_reverb => FXReverb.new,
      :fx_level => FXLevel.new,
      :fx_echo => FXEcho.new,
      :fx_slicer => FXSlicer.new,
      :fx_techno => FXTechno.new,
      :fx_compressor => FXCompressor.new,
      :fx_rlpf => FXRLPF.new,
      :fx_norm_rlpf => FXNormRLPF.new,
      :fx_rhpf => FXRHPF.new,
      :fx_norm_rhpf => FXNormRHPF.new,
      :fx_hpf => FXHPF.new,
      :fx_norm_hpf => FXNormHPF.new,
      :fx_lpf => FXLPF.new,
      :fx_norm_lpf => FXNormLPF.new,
      :fx_normaliser => FXNormaliser.new


      }

    def self.get_info(synth_name)
      @@synth_infos[synth_name.to_sym]
    end
  end
end
