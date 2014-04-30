module SonicPi

  class BaseInfo
    def doc
       "Please write documentation!"
    end

    def arg_defaults
      raise "please implement me!"
    end

    def name
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
          :doc => "The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played (due to compression.)",
          :validations => [v_positive(:amp)],
          :modulatable => true
        },

        :amp_slide =>
        {
          :doc => "Amount of time (in seconds) for the amplitude (amp) to change. A long slide value means that the amp takes a long time to slide from the previous amplitude to the new amplitude. A slide of 0 means that the amplitude instantly changes to the new amplitude.",
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
          :doc => generic_slide_doc(:detune),
          :validations => [v_positive(:detune_slide)],
          :modulatable => true
        },

        :mod_rate =>
        {
          :doc => "Number of times per second that the note switches between the two notes.",
          :validations => [v_positive(:mod_rate)],
          :modulatable => true
        },

        :mod_rate_slide =>
        {
          :doc => generic_slide_doc(:mod_rate),
          :validations => [v_positive(:mod_rate_slide)],
          :modulatable => true
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
          :modulatable => true
        },

        :mod_width =>
        {
          :doc => "The phase width of the modulation. Represents how even the gap between modulations is.",
          :validations => [v_between_exclusive(:mod_width, 0, 1)],
          :modulatable => true
        },

        :mod_width_slide =>
        {
          :doc => generic_slide_doc(:mod_width),
          :validations => [v_positive(:mod_width_slide)],
          :modulatable => true
        }

      }
    end

    def specific_arg_info
      {}
    end

  end

  class SynthInfo < BaseInfo
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
        :attack => 0.0,
        :sustain => 0,
        :release => 0.2
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
      "A sine wave with a fundamental frequency which is modulated at audio rate by another sine wave with a specific modulation division and depth."
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
          :doc => "Modifies the frequency of the modulator oscillator relative to the carrier. Don't worry too much about what this means - just try different numbers out!",
          :validations => [],
          :modulatable => true
        },

        :divisor_slide =>
        {
          :doc => generic_slide_doc(:divisor),
          :validations => [v_positive(:divisor_slide)],
          :modulatable => true
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
      "A saw wave which modulates between two separate notes."
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
        :mod_range_slide => 0,
        :mod_width => 0.5,
        :mod_width_slide => 0
      }
    end
  end

  class ModSawS < SynthInfo
    def name
      "Simple Modulated Saw Wave"
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
      "Modulated Sine Wave"
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
      "Simple Modualted Sine Wave"
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
      "Simple Modulated Triangle Wave"
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
        :mod_rate_slide => 0,
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
      "Simple Modulated Pulse"
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
      "TB-303 Emulation"
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
    def name
      "Supersaw"
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

  class Zawa < SynthInfo
    def name
      "Zawa"
    end

    def doc
      "Write me"
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
        :release => 1,

        :cutoff => 100,
        :cutoff_slide => 0,
        :rate => 1,
        :rate_slide => 0,
        :depth => 1.5,
        :depth_slide => 0

      }

    end
  end


  class Prophet < SynthInfo
    def name
      "The Prophet"
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

  class StudioInfo < SynthInfo

  end

  class BasicMonoPlayer < StudioInfo
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

  class MonoPlayer < StudioInfo
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
          :validations => [],
          :modulatable => false
        },

        :start =>
        {
          :doc => "",
          :validations => [v_positive(:start), v_between_inclusive(:start, 0, 1)],
          :modulatable => false
        },

        :finish =>
        {
          :doc => "",
          :validations => [v_positive(:finish), v_between_inclusive(:finish, 0, 1)],
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

  class BaseMixer < BaseInfo

  end

  class BasicMixer < BaseMixer
    def name
      "Basic Mixer"
    end

    def arg_defaults
      {
        :amp => 1,
        :amp_slide => 0.2
      }
    end

  end

  class FXInfo < BaseInfo

  end

  class FXReverb < FXInfo
    def name
      "FX Reverb"
    end

    def arg_defaults
      {
        :mix => 0.4,
        :mix_slide => 0,
        :room => 0.6,
        :room_slide => 0,
        :damp => 0.5,
        :damp_slide => 0
      }
    end
  end

  class FXLevel < FXInfo
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

  class FXEcho < FXInfo
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

    def kill_delay(args_h)
      args_h[:decay] || arg_defaults[:decay]
    end

  end

  class FXSlicer < FXInfo
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


  class FXIXITechno < FXInfo
    def name
      "FX Techno from IXI Lang"
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


  class FXCompressor < FXInfo
    def name
      "FX Compressor"
    end

    def arg_defaults
      {
        :amp => 1,
        :amp_slide => 0,
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


  class FXRLPF < FXInfo
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

  class FXRHPF < FXInfo
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

  class FXLPF < FXInfo
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

  class FXHPF < FXInfo
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

  class FXNormaliser < FXInfo
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

  class FXDistortion < FXInfo
    def name
      "FX Distortion"
    end

    def arg_defaults
      {
        :distort => 0.5,
        :distort_slide => 0
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
        :desc => "Percurssive Sounds",
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
      :saw_beep => SawBeep.new,
      :beep => Beep.new,
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
      :zawa => Zawa.new,
      :mono_player => MonoPlayer.new,
      :stereo_player => StereoPlayer.new,

      :basic_mono_player => BasicMonoPlayer.new,
      :basic_stereo_player => BasicStereoPlayer.new,
      :basic_mixer => BasicMixer.new,

      :fx_reverb => FXReverb.new,
      :fx_replace_reverb => FXReverb.new,
      :fx_level => FXLevel.new,
      :fx_replace_level => FXLevel.new,
      :fx_echo => FXEcho.new,
      :fx_replace_echo => FXEcho.new,
      :fx_slicer => FXSlicer.new,
      :fx_replace_slicer => FXSlicer.new,
      :fx_ixi_techno => FXIXITechno.new,
      :fx_replace_ixi_techno => FXIXITechno.new,
      :fx_compressor => FXCompressor.new,
      :fx_replace_compressor => FXCompressor.new,
      :fx_rlpf => FXRLPF.new,
      :fx_replace_rlpf => FXRLPF.new,
      :fx_norm_rlpf => FXNormRLPF.new,
      :fx_replace_norm_rlpf => FXNormRLPF.new,
      :fx_rhpf => FXRHPF.new,
      :fx_replace_rhpf => FXRHPF.new,
      :fx_norm_rhpf => FXNormRHPF.new,
      :fx_replace_norm_rhpf => FXNormRHPF.new,
      :fx_hpf => FXHPF.new,
      :fx_replace_hpf => FXHPF.new,
      :fx_norm_hpf => FXNormHPF.new,
      :fx_replace_norm_hpf => FXNormHPF.new,
      :fx_lpf => FXLPF.new,
      :fx_replace_lpf => FXLPF.new,
      :fx_norm_lpf => FXNormLPF.new,
      :fx_replace_norm_lpf => FXNormLPF.new,
      :fx_normaliser => FXNormaliser.new,
      :fx_replace_normaliser => FXNormaliser.new,
      :fx_distortion => FXDistortion.new,
      :fx_replace_distortion => FXDistortion.new


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
      get_all.each do |k, v|
        next unless v.is_a? klass
        doc = ""
        doc << "<h2> " << v.name << "</h2>"
        doc << "<h2><pre>use_synth"
        mk = key_mod ? key_mod.call(k) : k
        doc << " :#{mk}</pre></h2>"

        doc << "<h4><pre>{"
        arglist = []
        v.arg_info.each do |ak, av|
          arglist << "#{ak}: #{av[:default]}"
        end
        doc << arglist.join(", ")
        doc << "}</pre></h4>"

        doc << "<h3>"
        doc << "  " << v.doc << "</h3>"

        doc << "<h3>Argument Documentation:</h3>"
        doc << "<ul>"

        v.arg_info.each do |ak, av|
          doc << "  <li> #{ak}:<ul>"
          doc << "    <li> doc: #{av[:doc] || 'write me'}</li>"
          doc << "    <li> default: #{av[:default]}</li>"
          doc << "    <li> constraints: #{av[:constraints].empty? ? "none" : av[:constraints].join(",")}</li>"
          doc << "    <li>#{av[:modulatable] ? "May be changed whilst playing" : "Can not be changed once set"}</li></ul>"
        end
        doc << "</li></ul>"
        res[v.name] = doc
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

    def self.synth_doc_markdown
      info_doc_markdown("Synths", SynthInfo)
    end

    def self.fx_doc_markdown
      info_doc_markdown("FX", FXInfo, lambda{|k| k.to_s[3..-1]})
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
