module SonicPi
  class SynthInfo

    def self.get_info(synth_name)
      synth_classes =
        {
        :dull_bell => DullBell,
        :pretty_bell => PrettyBell
      }

      klass = synth_classes[synth_name.to_sym]
      klass.new
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
  end

  class Beep < SynthInfo
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
  end

  class DSaw < SynthInfo
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
end
