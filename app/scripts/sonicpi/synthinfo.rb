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
      info = arg_info
      args.keys.each do |a|
        a_sym = a.to_sym
        a_info = info[a_sym]
        validations = a_info[:validations] || []
        validations.each do |vali|
          raise "Value of argument #{a_sym.inspect}  must a number, got #{args[a_sym].inspect}." unless args[a_sym].is_a? Numeric
          v, m = vali
          raise "Value of argument #{a_sym.inspect} #{m}, got #{args[a_sym].inspect}." unless v.call(args)
        end
      end
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

    def v_between_inclusive(arg, min, max)
      [lambda{|args| args[arg] >= min && args[arg] <= max}, "must be a value between #{min} and #{max}"]
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
          :validations => [v_positive(:note)]
        },


        :amp =>
        {
          :doc => "The amplitude of the sound. Typically a value between 0 and 1. Higher amplitudes may be used, but won't make the sound louder, it will just reduce the quality of all the sounds currently being played.",
          :validations => [v_positive(:note)]
        },


        :pan =>
        {

          :doc => "Position of sound in stereo. With headphones on, this means how much of the sound is in the left ear, and how much is in the right ear. With a value of -1, the soundis completely in the left ear, a value of 0 puts the sound equally in both ears and a value of 1 puts the sound in the right ear. Values in between -1 and 1 move the sound accordingly.",
          :validations => [v_between_inclusive(:pan, -1, 1)]
        },


        :attack =>
        {
          :doc => "Amount of time (in seconds) for sound to reach full amplitude. A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently. Full length of sound is attack + sustain + release.",
          :validations => [v_positive(:attack)]
        },


        :sustain =>
        {
          :doc => "Amount of time (in seconds) for sound to remain at full amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + sustain + release.",
          :validations => [v_positive(:sustain)]
        },


        :release =>
        {
          :doc => "Amount of time (in seconds) for sound to move from full amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently. Full length of sound is attack + sustain + release.",
          :validations => [v_positive(:release)]
        },


        :slide =>
        {
          :doc => "Amount of time (in seconds) for the note to change. A long slide value means that the note takes a long time to slide from the previous note to the new note. A slide of 0 means that the note instantly changes to the new note.",
          :validations => [v_positive(:slide)]
        },


        :cutoff =>
        {
          :doc => "MIDI note representing the highest frequences allowed to be present in the sound. A low value like 30 makes the sound round and dull, a high value like 100 makes the sound buzzy and crispy.",
          :validations => [v_positive(:cutoff), v_less_than(:cutoff, 130)]
        },

        :cutoff_slide =>
        {
          :doc => "Amount of time (in seconds) for the cutoff value to change. A long cutoff_slide value means that the cutoff takes a long time to slide from the previous value to the new value. A cutoff_slide of 0 means that the cutoff instantly changes to the new value.",
          :validations => [v_positive(:cutoff_slide)]
        },

        :detune =>
        {
          :doc => "Distance (in MIDI notes) between components of sound. Affects thickness, sense of tuning and harmony. Tiny values such as 0.1 create a thick sound. Larger values such as 0.5 make the tuning sound strange. Even bigger values such as 5 create chord-like sounds.",
          :validations => []
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
        :amp => 1,
        :pan => 0,
        :attack => 0.01,
        :sustain => 0,
        :release => 1,
        :slide => 0
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
        :amp => 1,
        :pan => 0,
        :attack => 0.1,
        :sustain => 0,
        :release => 0.3,
        :slide => 0
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
        :amp => 1,
        :pan => 0,
        :attack => 0.1,
        :sustain => 0,
        :release => 0.3,
        :slide => 0,

        :cutoff => 100,
        :cutoff_slide => 0,
        :detune => 0.1
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
        :amp => 1,
        :pan => 0,
        :attack => 1,
        :sustain => 0,
        :release => 1,
        :slide => 0,

        :divisor => 2,
        :depth => 1,
        :div_slide => 0,
        :depth_slide => 0
      }
    end

    def specific_arg_info
      {
        :divisor =>
        {
          :doc => "",
          :validations => []
        },

        :depth =>
        {
          :doc => "",
          :validations => []
        },

        :div_slide =>
        {
          :doc => "",
          :validations => [v_positive(:div_slide)]
        },

        :depth_slide =>
        {
          :doc => "",
          :validations => [v_positive(:depth_slide)]
        }
      }

    end
  end

  class ModSaw
    def name
      "Modulated Saw Wave"
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
        :mod_width => 0.5
      }
    end
  end

  class ModSawS
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

  class ModDSaw
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

  class ModDSawS
    def name
      "Modulated Detuned Saw Waves Simple"
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
        :mod_width => 0.5,
        :detune => 0.1
      }
    end
  end

  class ModSine
    def name
      "Modualted Sine Wave"
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
        :mod_width => 0.5
      }
    end
  end

  class ModSineS
    def name
      "Modualted Sine Wave Simple"
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

  class ModTri
    def name
      "Modulated Triangle Wave"
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
        :mod_width => 0.5
      }
    end
  end

  class ModTriS
    def name
      "Modulated Triangle Wave Simple"
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

  class ModPulse
    def name
      "Modulated Pulse"
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
        :pulse_width => 0.5
      }
    end
  end

  class ModPulseS
    def name
      "Modulated Pulse Simple"
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
        :mod_width => 0.5,
        :pulse_width => 0.5
      }
    end
  end

  class TB303
    def name
      "tb-303"
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

        :cutoff => 80,
        :cutoff_min => 30,
        :res => 0.2,
        :wave => 0,
        :pulse_width => 0.5
      }
    end

    def specific_arg_info
      {

        :cutoff =>
        {
          :doc => "",
          :validations => [v_positive(:cutoff), v_less_than(:cutoff, 130)]
        },

        :cutoff_min =>
        {
          :doc => "",
          :validations => [v_positive(:cutoff), v_less_than(:cutoff, 130)]
        },

        :wave =>
        {
          :doc => "Wave type - 0 saw, 1 pulse",
          :validations => [v_one_of(:wave, [0, 1])]
        },

        :pulse_width =>
        {
          :doc => "Only valid if wave is type pulse.",
l=          :validations => [v_positive(:pulse_width)]
        }

      }
    end
  end

  class Supersaw
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

        :cutoff => 130,
        :cutoff_slide => 0,
        :res => 0.3

      }
    end
  end

  class SupersawS
    def name
      "Supersaw Simple"
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
        :slide => 0
      }
    end

  end


  class Prophet
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
        :amp => 1,
        :pan => 0,
        :attack => 0.01,
        :sustain => 0,
        :release => 2,
        :slide => 0,

        :cutoff => 110,
        :cutoff_slide => 0,
        :res => 0.3
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
        :prophet => Prophet.new
      }

    def self.get_info(synth_name)
      @@synth_infos[synth_name.to_sym]
    end
  end
end
