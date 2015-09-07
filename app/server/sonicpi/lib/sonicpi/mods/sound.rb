#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++
require 'tmpdir'
require 'fileutils'
require 'thread'
require 'net/http'
require "hamster/set"
require "hamster/hash"
require_relative "../blanknode"
require_relative "../chainnode"
require_relative "../fxnode"
require_relative "../fxreplacenode"
require_relative "../note"
require_relative "../scale"
require_relative "../chord"
require_relative "../chordgroup"
require_relative "../synthtracker"
require_relative "../docsystem"
require_relative "../version"
require_relative "../tuning"

class Symbol
  def -(other)
    return self if (self == :r) || (self == :rest)
    SonicPi::Note.resolve_midi_note_without_octave(self) - SonicPi::Note.resolve_midi_note_without_octave(other)
  end

  def +(other)
    return self if (self == :r) || (self == :rest)
    SonicPi::Note.resolve_midi_note_without_octave(self) + SonicPi::Note.resolve_midi_note_without_octave(other)
  end
end

class NilClass
  def -(other)
    return nil
  end

  def +(other)
    return nil
  end
end

module SonicPi
  module Mods
    module Sound

      include SonicPi::Util
      include SonicPi::DocSystem

      DEFAULT_PLAY_OPTS = {amp:       "The amplitude of the note",
        amp_slide: "The duration in beats for amplitude changes to take place",
        pan:       "The stereo position of the sound. -1 is left, 0 is in the middle and 1 is on the right. You may use a value in between -1 and 1 such as 0.25",
        pan_slide: "The duration in beats for the pan value to change",
        attack: "Amount of time (in beats) for sound to reach full amplitude (attack_level). A short attack (i.e. 0.01) makes the initial part of the sound very percussive like a sharp tap. A longer attack (i.e 1) fades the sound in gently.",
        decay: "Amount of time (in beats) for the sound to move from full amplitude (attack_level) to the sustain amplitude (sustain_level).",
        sustain:  "Amount of time (in beats) for sound to remain at sustain level amplitude. Longer sustain values result in longer sounds. Full length of sound is attack + decay + sustain + release.",
        release:   "Amount of time (in beats) for sound to move from sustain level amplitude to silent. A short release (i.e. 0.01) makes the final part of the sound very percussive (potentially resulting in a click). A longer release (i.e 1) fades the sound out gently.",
        attack_level: "Amplitude level reached after attack phase and immediately before decay phase",
        decay_level: "Amplitude level reached after decay phase and immediately before sustain phase. Defaults to sustain_level unless explicitly set",
        sustain_level: "Amplitude level reached after decay phase and immediately before release phase.",
        env_curve: "Select the shape of the curve between levels in the envelope. 1=linear, 2=exponential, 3=sine, 4=welch, 6=squared, 7=cubed",
        slide:     "Default slide time in beats for all slide opts. Individually specified slide opts will override this value" }



      def self.included(base)
        base.instance_exec {alias_method :sonic_pi_mods_sound_initialize_old, :initialize}

        base.instance_exec do
          define_method(:initialize) do |*splat, &block|
            sonic_pi_mods_sound_initialize_old *splat, &block
            hostname, port, msg_queue, max_concurrent_synths = *splat

            @mod_sound_home_dir = File.expand_path('~/')
            @simple_sampler_args = [:amp, :amp_slide, :amp_slide_shape, :amp_slide_curve, :pan, :pan_slide, :pan_slide_shape, :pan_slide_curve, :cutoff, :cutoff_slide, :cutoff_slide_shape, :cutoff_slide_curve, :res, :res_slide, :res_slide_shape, :res_slide_curve, :rate]

            @tuning = Tuning.new

            @blank_node = BlankNode.new
            @job_proms_queues = {}
            @job_proms_queues_mut = Mutex.new

            @job_proms_joiners = {}

            @sample_paths_cache = {}

            @JOB_GROUPS_A = Atom.new(Hamster.hash)
            @JOB_GROUP_MUTEX = Mutex.new
            @JOB_FX_GROUP_MUTEX = Mutex.new
            @JOB_FX_GROUPS_A = Atom.new(Hamster.hash)
            @JOB_MIXERS_A = Atom.new(Hamster.hash)
            @JOB_MIXERS_MUTEX = Mutex.new
            @JOB_BUSSES_A = Atom.new(Hamster.hash)
            @JOB_BUSSES_MUTEX = Mutex.new
            @mod_sound_studio = Studio.new(hostname, port, msg_queue, max_concurrent_synths)

            @life_hooks.on_init do |job_id, payload|
              @job_proms_queues_mut.synchronize do
                @job_proms_queues[job_id] = Queue.new
                joiner = job_proms_joiner(job_id)
                @job_proms_joiners[job_id] = joiner
              end
            end


            @life_hooks.on_killed do |job_id, payload|
              q = @job_proms_queues[job_id]
              q << :job_finished if q
            end

            @life_hooks.on_completed do |job_id, payload|

              ## At this point we can be assured that no more threads
              ## are running for this particular job. We therefore
              ## don't have to worry about concurrency issues.
              joiner = @job_proms_joiners[job_id]
              if joiner

                @job_proms_queues[job_id] << :job_finished
                joiner.get
                @job_proms_queues_mut.synchronize do
                  @job_proms_joiners.delete job_id
                end
              end
            end

            @life_hooks.on_exit do |job_id, payload|
              Thread.new do
                Thread.current.thread_variable_set(:sonic_pi_spider_start_time, payload[:start_t])
                Thread.current.thread_variable_set(:sonic_pi_thread_group, "job_remover-#{job_id}")
                Thread.current.priority = -10
                shutdown_job_mixer(job_id)
                kill_job_group(job_id)
                kill_fx_job_group(job_id)
                free_job_bus(job_id)
              end

            end

            @events.add_handler("/exit", @events.gensym("/mods-sound-exit")) do |payload|
              @mod_sound_studio.exit
              nil
            end
          end
        end
      end


      def midi_notes(*args)
        args = args.map {|a| note(a)}
        SonicPi::Core::RingVector.new(args)
      end
      doc name:           :midi_notes,
          introduced:     Version.new(2,7,0),
          summary:        "Create a ring buffer of midi note numbers",
          args:           [[:list, :array]],
          returns:        :ring,
          opts:           nil,
          accepts_block:  false,
          doc:            "Create a new immutable ring buffer of notes from args. Indexes wrap around positively and negatively. Final ring consists only of MIDI numbers and nil.",
          examples:       [
        "(midi_notes :d3, :d4, :d5) #=> (ring 50, 62, 74)",
        "(midi_notes :d3, 62,  nil) #=> (ring 50, 62, nil)"
       ]


      def rest?(n)
        case n
        when Numeric
          return false
        when Symbol
          return n == :r || n == :rest
        when NilClass
          return true
        when Hash
          if n.has_key?(:note)
            note = n[:note]
            return (note.nil? || note == :r || note == :rest)
          else
            return false
          end
        else
          return false
        end
      end
      doc name:          :rest?,
          introduced:    Version.new(2,1,0),
          summary:       "Determine if note or args is a rest",
          doc:           "Given a note or an args map, returns true if it represents a rest and false if otherwise",
          args:          [[:note_or_args, :number_symbol_or_map]],
          accepts_block: false,
          examples:      ["puts rest? nil # true",
        "puts rest? :r # true",
        "puts rest? :rest # true",
        "puts rest? 60 # false",
        "puts rest? {} # false",
        "puts rest? {note: :rest} # true",
        "puts rest? {note: nil} # true",
        "puts rest? {note: 50} # false"]




      def use_timing_warnings(v, &block)
        raise "use_timing_warnings does not work with a do/end block. Perhaps you meant with_timing_warnings" if block
        Thread.current.thread_variable_set(:sonic_pi_mod_sound_disable_timing_warnings, !v)
      end




      def with_timing_warnings(v, &block)
        raise "with_debug requires a do/end block. Perhaps you meant use_debug" unless block
        current = Thread.current.thread_variable_get(:sonic_pi_mod_sound_disable_timing_warnings)
        Thread.current.thread_variable_set(:sonic_pi_mod_sound_disable_timing_warnings, !v)
        block.call
        Thread.current.thread_variable_set(:sonic_pi_mod_sound_disable_timing_warnings, current)
      end




      def use_sample_bpm(sample_name, *args)
        args_h = resolve_synth_opts_hash_or_array(args)
        num_beats = args_h[:num_beats] || 1

        # Don't use sample_duration as that is stretched to the current
        # bpm!
        sd = load_sample(sample_name).duration
        use_bpm(num_beats * (60.0 / sd))
      end
      doc name:           :use_sample_bpm,
          introduced:     Version.new(2,1,0),
          summary:        "Sample-duration-based bpm modification",
          doc:            "Modify bpm so that sleeping for 1 will sleep for the duration of the sample.",
          args:           [[:string_or_number, :sample_name_or_duration]],
          opts:           {:num_beats => "The number of beats within the sample. By default this is 1."},
          accepts_block:  false,
          examples:       ["use_sample_bpm :loop_amen  #Set bpm based on :loop_amen duration

live_loop :dnb do
  sample :bass_dnb_f
  sample :loop_amen
  sleep 1                  #`sleep`ing for 1 actually sleeps for duration of :loop_amen
end",
        "
use_sample_bpm :loop_amen, num_beats: 4  # Set bpm based on :loop_amen duration
                                         # but also specify that the sample duration
                                         # is actually 4 beats long.

live_loop :dnb do
  sample :bass_dnb_f
  sample :loop_amen
  sleep 4                  #`sleep`ing for 4 actually sleeps for duration of :loop_amen
                           # as we specified that the sample consisted of
                           # 4 beats
end"]




      def with_sample_bpm(sample_name, *args, &block)
        raise "with_sample_bpm must be called with a do/end block" unless block
        args_h = resolve_synth_opts_hash_or_array(args)
        num_beats = args_h[:num_beats] || 1
        # Don't use sample_duration as that is stretched to the current
        # bpm!
        sd = load_sample(sample_name).duration
        with_bpm(num_beats * (60.0 / sd), &block)
      end
      doc name:           :with_sample_bpm,
          introduced:     Version.new(2,1,0),
          summary:        "Block-scoped sample-duration-based bpm modification",
          doc:            "Block-scoped modification of bpm so that sleeping for 1 will sleep for the duration of the sample.",
          args:           [[:string_or_number, :sample_name_or_duration]],
          opts:           {:num_beats => "The number of beats within the sample. By default this is 1."},
          accepts_block:  true,
          requires_block: true,
          examples:       ["
live_loop :dnb do
  with_sample_bpm :loop_amen do #Set bpm based on :loop_amen duration
    sample :bass_dnb_f
    sample :loop_amen
    sleep 1                     #`sleep`ing for 1 sleeps for duration of :loop_amen
  end
end",
        "live_loop :dnb do
  with_sample_bpm :loop_amen, num_beats: 4 do # Set bpm based on :loop_amen duration
                                              # but also specify that the sample duration
                                              # is actually 4 beats long.
    sample :bass_dnb_f
    sample :loop_amen
    sleep 4                     #`sleep`ing for 4 sleeps for duration of :loop_amen
                                # as we specified that the sample consisted of
                                # 4 beats
  end
end"]




      def use_arg_bpm_scaling(bool, &block)
        raise "use_arg_bpm_scaling does not work with a block. Perhaps you meant with_arg_bpm_scaling" if block
        Thread.current.thread_variable_set(:sonic_pi_spider_arg_bpm_scaling, bool)
      end
      doc name:           :use_arg_bpm_scaling,
          introduced:     Version.new(2,0,0),
          summary:        "Enable and disable BPM scaling",
          doc:            "Turn synth argument bpm scaling on or off for the current thread. This is on by default. Note, using `rt` for args will result in incorrect times when used after turning arg bpm scaling off.",
          args:           [[:bool, :boolean]],
          opts:           nil,
          accepts_block:  false,
          examples:       ["
use_bpm 120
play 50, release: 2 # release is actually 1 due to bpm scaling
sleep 2             # actually sleeps for 1 second
use_arg_bpm_scaling false
play 50, release: 2 # release is now 2
sleep 2             # still sleeps for 1 second",

        "                       # Interaction with rt
use_bpm 120
play 50, release: rt(2) # release is 2 seconds
sleep rt(2)             # sleeps for 2 seconds
use_arg_bpm_scaling false
play 50, release: rt(2) # ** Warning: release is NOT 2 seconds! **
sleep rt(2)             # still sleeps for 2 seconds"]





      def with_arg_bpm_scaling(bool, &block)
        raise "with_arg_bpm_scaling must be called with a do/end block. Perhaps you meant use_arg_bpm_scaling" unless block
        current_scaling = Thread.current.thread_variable_get(:sonic_pi_spider_arg_bpm_scaling)

        Thread.current.thread_variable_set(:sonic_pi_spider_arg_bpm_scaling, bool)
        block.call
        Thread.current.thread_variable_set(:sonic_pi_spider_arg_bpm_scaling, current_scaling)
      end
      doc name:           :with_arg_bpm_scaling,
          introduced:     Version.new(2,0,0),
          summary:        "Block-level enable and disable BPM scaling",
          doc:            "Turn synth argument bpm scaling on or off for the supplied block. Note, using `rt` for args will result in incorrect times when used within this block.",
          args:           [],
          opts:           nil,
          accepts_block:  true,
          requires_block: true,
          examples:       ["use_bpm 120
play 50, release: 2 # release is actually 1 due to bpm scaling
with_arg_bpm_scaling false do
  play 50, release: 2 # release is now 2
end",

        "                         # Interaction with rt
use_bpm 120
play 50, release: rt(2)   # release is 2 seconds
sleep rt(2)               # sleeps for 2 seconds
with_arg_bpm_scaling false do
  play 50, release: rt(2) # ** Warning: release is NOT 2 seconds! **
  sleep rt(2)             # still sleeps for 2 seconds
end"]


      def pitch_ratio(*args)
        raise "The fn pitch_ratio has been renamed. Please use the new name: pitch_to_ratio"
      end

      def pitch_to_ratio(m)
        2.0 ** (m.to_f / 12.0)
      end
      doc name:          :pitch_to_ratio,
          introduced:    Version.new(2,5,0),
          summary:       "relative MIDI pitch to frequency ratio",
          doc:           "Convert a midi note to a ratio which when applied to a frequency will scale the frequency by the number of semitones. Useful for changing the pitch of a sample by using it as a way of generating the rate.",
          args:          [[:pitch, :midi_number]],
          opts:          nil,
          accepts_block: false,
          examples:      [
        "pitch_to_ratio 12 #=> 2.0",
        "pitch_to_ratio 1 #=> 1.05946",
        "pitch_to_ratio -12 #=> 0.5",
        "sample :ambi_choir, rate: pitch_to_ratio(3) # Plays :ambi_choir 3 semitones above default.",
        "
# Play a chromatic scale of semitones
(range 0, 16).each do |n|                  # For each note in the range 0->16
  sample :ambi_choir, rate: pitch_to_ratio(n) # play :ambi_choir at the relative pitch
  sleep 0.5                                # and wait between notes
end"
      ]




      def ratio_to_pitch(r)
        12.0 * Math.log2(r.abs.to_f)
      end
      doc name:          :ratio_to_pitch,
          introduced:    Version.new(2,7,0),
          summary:       "relative frequency ratio to MIDI pitch",
          doc:           "Convert a frequency ratio to a midi note which when added to a note will transpose the note to match the frequency ratio.",
          args:          [[:ratio, :number]],
          opts:          nil,
          accepts_block: false,
          examples:      [
        "ratio_to_pitch 2 #=> 12.0",
        "ratio_to_pitch 0.5 #=> -12.0"

      ]




      def midi_to_hz(n)
        n = note(n) unless n.is_a? Numeric
        440.0 * (2 ** ((n - 69) / 12.0))
      end
      doc name:          :midi_to_hz,
         introduced:    Version.new(2,0,0),
         summary:       "MIDI to Hz conversion",
         doc:           "Convert a midi note to hz",
         args:          [[:note, :symbol_or_number]],
         opts:          nil,
         accepts_block: false,
         examples:      ["midi_to_hz(60) #=> 261.6256"]




      def hz_to_midi(freq)
        (12 * (Math.log(freq * 0.0022727272727) / Math.log(2))) + 69
      end
      doc name:          :hz_to_midi,
          introduced:    Version.new(2,0,0),
          summary:       "Hz to MIDI conversion",
          doc:           "Convert a frequency in hz to a midi note. Note that the result isn't an integer and there is a potential for some very minor rounding errors.",
          args:          [[:freq, :number]],
          opts:          nil,
          accepts_block: false,
          examples:      ["hz_to_midi(261.63) #=> 60.0003"]
!



      def set_control_delta!(t)
        @mod_sound_studio.control_delta = t
        __info "Control delta set to #{t}"
      end
      doc name:          :set_control_delta!,
          introduced:    Version.new(2,1,0),
          summary:       "Set control delta globally",
          doc:           "Specify how many seconds between successive modifications (i.e. trigger then controls) of a specific node on a specific thread. Set larger if you are missing control messages sent extremely close together in time.",
          args:          [[:time, :number]],
          opts:          nil,
          modifies_env: true,
          accepts_block: false,
          examples:      [
        "
set_control_delta! 0.1                 # Set control delta to 0.1

s = play 70, release: 8, note_slide: 8 # Play a note and set the slide time
control s, note: 82                    # immediately start sliding note.
                                       # This control message might not be
                                       # correctly handled as it is sent at the
                                       # same virtual time as the trigger.
                                       # If you don't hear a slide, try increasing the
                                       # control delta until you do."]




      def set_sched_ahead_time!(t)
        @mod_sound_studio.sched_ahead_time = t
        __info "Schedule ahead time set to #{t}"
      end
      doc name:          :set_sched_ahead_time!,
          introduced:    Version.new(2,0,0),
          summary:       "Set sched ahead time globally",
          doc:           "Specify how many seconds ahead of time the synths should be triggered. This represents the amount of time between pressing 'Run' and hearing audio. A larger time gives the system more room to work with and can reduce performance issues in playing fast sections on slower platforms. However, a larger time also increases latency between modifying code and hearing the result whilst live coding.",
          args:          [[:time, :number]],
          opts:          nil,
          modifies_env: true,
          accepts_block: false,
          examples:      ["set_sched_ahead_time! 1 # Code will now run approximately 1 second ahead of audio."]




      def use_debug(v, &block)
        raise "use_debug does not work with a do/end block. Perhaps you meant with_debug" if block
        Thread.current.thread_variable_set(:sonic_pi_mod_sound_synth_silent, !v)
      end
      doc name:          :use_debug,
          introduced:    Version.new(2,0,0),
          summary:       "Enable and disable debug",
          doc:           "Enable or disable messages created on synth triggers. If this is set to false, the synths will be silent until debug is turned back on. Silencing debug messages can reduce output noise and also increase performance on slower platforms. See `with_debug` for setting the debug value only for a specific `do`/`end` block.",
          args:          [[:true_or_false, :boolean]],
          opts:          nil,
          accepts_block: false,
          examples:      ["use_debug true # Turn on debug messages", "use_debug false # Disable debug messages"]




      def with_debug(v, &block)
        raise "with_debug requires a do/end block. Perhaps you meant use_debug" unless block
        current = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_silent)
        Thread.current.thread_variable_set(:sonic_pi_mod_sound_synth_silent, !v)
        block.call
        Thread.current.thread_variable_set(:sonic_pi_mod_sound_synth_silent, current)
      end
      doc name:          :with_debug,
          introduced:    Version.new(2,0,0),
          summary:       "Block-level enable and disable debug",
          doc:           "Similar to use_debug except only applies to code within supplied `do`/`end` block. Previous debug value is restored after block.",
          args:          [[:true_or_false, :boolean]],
          opts:          nil,
          accepts_block: true,
          requires_block: true,
          examples:      ["
# Turn on debugging:
use_debug true

play 80 # Debug message is sent

with_debug false do
  #Debug is now disabled
  play 50 # Debug message is not sent
  sleep 1
  play 72 # Debug message is not sent
end

# Debug is re-enabled
play 90 # Debug message is sent

"]




      def use_arg_checks(v, &block)
        raise "use_arg_checks does not work with a do/end block. Perhaps you meant with_arg_checks" if block

        Thread.current.thread_variable_set(:sonic_pi_mod_sound_check_synth_args, !!v)
      end
      doc name:          :use_arg_checks,
          introduced:    Version.new(2,0,0),
          summary:       "Enable and disable arg checks",
          doc:           "When triggering synths, each argument is checked to see if it is sensible. When argument checking is enabled and an argument isn't sensible, you'll see an error in the debug pane. This setting allows you to explicitly enable and disable the checking mechanism. See with_arg_checks for enabling/disabling argument checking only for a specific `do`/`end` block.",
          args:          [[:true_or_false, :boolean]],
          opts:          nil,
          accepts_block: false,
          examples:      ["
play 50, release: 5 # Args are checked
use_arg_checks false
play 50, release: 5 # Args are not checked"]




      def with_arg_checks(v, &block)
        raise "with_arg_checks requires a do/end block. Perhaps you meant use_arg_checks" unless block

        current = Thread.current.thread_variable_get(:sonic_pi_mod_sound_check_synth_args)
        Thread.current.thread_variable_set(:sonic_pi_mod_sound_check_synth_args, v)
        block.call
        Thread.current.thread_variable_set(:sonic_pi_mod_sound_check_synth_args, current)
      end
      doc name:           :with_arg_checks,
          introduced:     Version.new(2,0,0),
          summary:        "Block-level enable and disable arg checks",
          doc:            "Similar to `use_arg_checks` except only applies to code within supplied `do`/`end` block. Previous arg check value is restored after block.",
          args:           [[:true_or_false, :boolean]],
          opts:           nil,
          accepts_block:  true,
          requires_block: true,
          examples:       ["
# Turn on arg checking:
use_arg_checks true

play 80, cutoff: 100 # Args are checked

with_arg_checks false do
  #Arg checking is now disabled
  play 50, release: 3 # Args are not checked
  sleep 1
  play 72             # Arg is not checked
end

# Arg checking is re-enabled
play 90 # Args are checked

"]




      def use_transpose(shift, &block)
        raise "use_transpose does not work with a do/end block. Perhaps you meant with_transpose" if block
        raise "Transpose value must be a number, got #{shift.inspect}" unless shift.is_a?(Numeric)
        Thread.current.thread_variable_set(:sonic_pi_mod_sound_transpose, shift)
      end
      doc name:          :use_transpose,
          introduced:    Version.new(2,0,0),
          summary:       "Note transposition",
          doc:           "Transposes your music by shifting all notes played by the specified amount. To shift up by a semitone use a transpose of 1. To shift down use negative numbers. See `with_transpose` for setting the transpose value only for a specific `do`/`end` block.",
          args:          [[:note_shift, :number]],
          opts:          nil,
          accepts_block: false,
          intro_fn:       true,
          examples:      ["
play 50 # Plays note 50
use_transpose 1
play 50 # Plays note 51",

        "
# You may change the transposition multiple times:
play 62 # Plays note 62
use_transpose -12
play 62 # Plays note 50
use_transpose 3
play 62 # Plays note 65"]




      def with_transpose(shift, &block)
        raise "with_transpose requires a do/end block. Perhaps you meant use_transpose" unless block
        raise "Transpose value must be a number, got #{shift.inspect}" unless shift.is_a?(Numeric)
        curr = Thread.current.thread_variable_get(:sonic_pi_mod_sound_transpose)
        Thread.current.thread_variable_set(:sonic_pi_mod_sound_transpose, shift)
        block.call
        Thread.current.thread_variable_set(:sonic_pi_mod_sound_transpose, curr)
      end
      doc name:           :with_transpose,
          introduced:     Version.new(2,0,0),
          summary:        "Block-level note transposition",
          doc:            "Similar to use_transpose except only applies to code within supplied `do`/`end` block. Previous transpose value is restored after block.",
          args:           [[:note_shift, :number]],
          opts:           nil,
          accepts_block:  true,
          requires_block: true,
          examples:       ["
use_transpose 3
play 62 # Plays note 65

with_transpose 12 do
  play 50 # Plays note 62
  sleep 1
  play 72 # Plays note 84
end

# Original transpose value is restored
play 80 # Plays note 83

"]

      def use_tuning(tuning, fundamental_note = :c, &block)
        raise "use_tuning does not work with a do/end block. Perhaps you meant with_tuning" if block
        raise "tuning value must be a symbol like :just or :equal, got #{tuning.inspect}" unless tuning.is_a?(Symbol)
        Thread.current.thread_variable_set(:sonic_pi_mod_sound_tuning, [tuning, fundamental_note])
      end
      doc name:          :use_tuning,
          introduced:    Version.new(2,6,0),
          summary:       "Use alternative tuning systems",
          doc:           "In most music we make semitones by dividing the octave into 12 equal parts, which is known as equal temperament. However there are lots of other ways to tune the 12 notes. This method adjusts each midi note into the specified tuning system. Because the ratios between notes aren't always equal, be careful to pick a centre note that is in the key of the music you're making for the best sound. Currently available tunings are :just, :pythagorean, :meantone and the default of :equal",
          args:          [[:tuning, :symbol], [:fundamental_note, :symbol_or_number]],
          opts:          nil,
          accepts_block: false,
          examples:      ["
play :e4 # Plays note 64
use_tuning :just, :c
play :e4 # Plays note 63.8631
# transparently changes midi notes too
play 64 # Plays note 63.8631",

        "
# You may change the tuning multiple times:
play 64 # Plays note 64
use_tuning :just
play 64 # Plays note 63.8631
use_tuning :equal
play 64 # Plays note 64"]



      def with_tuning(tuning, fundamental_note = :c, &block)
        raise "with_tuning requires a do/end block. Perhaps you meant use_tuning" unless block
        raise "tuning value must be a symbol like :just or :equal, got #{tuning.inspect}" unless tuning.is_a?(Symbol)
        curr_tuning, curr_fundamental = Thread.current.thread_variable_get(:sonic_pi_mod_sound_tuning)
        Thread.current.thread_variable_set(:sonic_pi_mod_sound_tuning, [tuning, fundamental_note])
        block.call
        Thread.current.thread_variable_set(:sonic_pi_mod_sound_tuning, [curr_tuning, curr_fundamental])
      end
      doc name:          :with_tuning,
          introduced:    Version.new(2,6,0),
          summary:       "Block-level tuning modification",
          doc:           "Similar to use_tuning except only applies to code within supplied `do`/`end` block. Previous tuning value is restored after block.",
          args:          [[:tuning, :symbol], [:fundamental_note, :symbol_or_number]],
          opts:          nil,
          accepts_block: true,
          examples:      ["
use_tuning :equal, :c
play :e4 # Plays note 64
with_tuning :just, :c do
  play :e4 # Plays note 63.8631
  sleep 1
  play :c4 # Plays note 60
end
# Original tuning value is restored
play :e4 # Plays note 64"]


      def use_synth(synth_name, &block)
        raise "use_synth does not work with a do/end block. Perhaps you meant with_synth" if block
        set_current_synth synth_name
      end
      doc name:          :use_synth,
          introduced:    Version.new(2,0,0),
          summary:       "Switch current synth",
          doc:           "Switch the current synth to `synth_name`. Affects all further calls to `play`. See `with_synth` for changing the current synth only for a specific `do`/`end` block.",
          args:          [[:synth_name, :symbol]],
          opts:          nil,
          accepts_block: false,
          intro_fn:       true,
          examples:      ["
play 50 # Plays with default synth
use_synth :mod_sine
play 50 # Plays with mod_sine synth"]




      def with_synth(synth_name, &block)
        raise "with_synth must be called with a do/end block. Perhaps you meant use_synth" unless block
        orig_synth = current_synth_name
        set_current_synth synth_name
        block.call
        set_current_synth orig_synth
      end
      doc name:           :with_synth,
          introduced:     Version.new(2,0,0),
          summary:        "Block-level synth switching",
          doc:            "Switch the current synth to `synth_name` but only for the duration of the `do`/`end` block. After the `do`/`end` block has completed, the previous synth is restored.",
          args:           [[:synth_name, :symbol]],
          opts:           nil,
          accepts_block:  true,
          requires_block: true,
          examples:      ["
play 50 # Plays with default synth
sleep 2
use_synth :supersaw
play 50 # Plays with supersaw synth
sleep 2
with_synth :saw_beep do
  play 50 # Plays with saw_beep synth
end
sleep 2
# Previous synth is restored
play 50 # Plays with supersaw synth
"]




      def recording_start
        if @mod_sound_studio.recording?
          __info "Already recording..."
        else
          __info "Start recording"
          tmp_dir = Dir.mktmpdir("sonic-pi")
          @tmp_path = File.expand_path("#{tmp_dir}/#{Random.rand(100000000)}.wav")
          @mod_sound_studio.recording_start @tmp_path
        end
      end
      doc name:          :recording_start,
          introduced:    Version.new(2,0,0),
          summary:       "Start recording",
          doc:           "Start recording all sound to a `.wav` file stored in a temporary directory.",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      [],
          hide:          true




      def recording_stop
        if @mod_sound_studio.recording?
          __info "Stop recording"
          @mod_sound_studio.recording_stop
        else
          __info "Recording already stopped"
        end
      end
      doc name:          :recording_stop,
          introduced:    Version.new(2,0,0),
          summary:       "Stop recording",
          doc:           "Stop current recording.",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      [],
          hide:          true




      def recording_save(filename)
        if @tmp_path && File.exists?(@tmp_path)
          FileUtils.mv(@tmp_path, filename)
          @tmp_path = nil
          __info "Saving recording to #{filename}"
        else
          __info "No recording to save"
        end
      end
      doc name:          :recording_save,
          introduced:    Version.new(2,0,0),
          summary:       "Save recording",
          doc:           "Save previous recording to the specified location",
          args:          [[:path, :string]],
          opts:          nil,
          accepts_block: false,
          examples:      [],
          hide:          true




      def recording_delete
        __info "Deleting recording..."
        FileUtils.rm @tmp_path if @tmp_path
      end
      doc name:          :recording_delete,
          doc:           "After using `recording_start` and `recording_stop`, a temporary file is created until you decide to use `recording_save`. If you've decided you don't want to save it you can use this method to delete the temporary file straight away, otherwise the operating system will take care of deleting it later.",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      [],
          hide:          true


      def set_mixer_invert_stereo!
        @mod_sound_studio.mixer_invert_stereo(true)
      end

      def set_mixer_standard_stereo!
        @mod_sound_studio.mixer_invert_stereo(false)
      end

      def set_mixer_stereo_mode!
        @mod_sound_studio.mixer_stereo_mode
      end

      def set_mixer_mono_mode!
        @mod_sound_studio.mixer_mono_mode
      end

      def set_mixer_control!(opts)
        @mod_sound_studio.mixer_control(opts)
      end

      def synth(synth_name, *args)
        ensure_good_timing!
        args_h = resolve_synth_opts_hash_or_array(args)

        if rest? args_h
          __delayed_message "synth #{synth_name.to_sym.inspect}, {note: :rest}"
          return nil
        end

        n = 52

        if args_h.has_key? :note
          n = args_h[:note]
          n = n.call if n.is_a? Proc
          n = note(n) unless n.is_a? Numeric
        end

        if shift = Thread.current.thread_variable_get(:sonic_pi_mod_sound_transpose)
          n += shift
        end

        n += args_h[:pitch].to_f

        if tuning_info = Thread.current.thread_variable_get(:sonic_pi_mod_sound_tuning)
          tuning_system, fundamental_sym = tuning_info
          if tuning_system != :equal
            n = @tuning.resolve_tuning(n, tuning_system, fundamental_sym)
          end
        end

        args_h[:note] = n


        trigger_inst synth_name, args_h
      end
      doc name:          :synth,
          introduced:    Version.new(2,0,0),
          summary:       "Trigger specific synth",
          doc:           "Trigger specified synth with given arguments. Bypasses current synth value, yet still honours synth defaults.",
          args:          [[:synth_name, :symbol]],
          opts:          {:slide => "Default slide time in beats for all slide opts. Individually specified slide opts will override this value"},
          accepts_block: false,
          examples:      ["
synth :fm, note: 60, amp: 0.5 # Play note 60 of the :fm synth with an amplitude of 0.5",

        "
use_synth_defaults release: 5
synth :dsaw, note: 50 # Play note 50 of the :dsaw synth with a release of 5"]



      def play(n, *args)
        ensure_good_timing!
        case n
        when Array, SonicPi::Core::RingVector
          return play_chord(n, *args)
        when Hash
          # Allow a single hash argument to function unsurprisingly
          args = n if args.empty?
        end

        n = note(n)

        synth_name = current_synth_name

        if n.nil?
          unless Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_silent)
            __delayed_message "synth #{synth_name.to_sym.inspect}, {note: :rest}"
          end

          return nil
        end

        init_args_h = {}
        args_h = resolve_synth_opts_hash_or_array(args)

        if shift = Thread.current.thread_variable_get(:sonic_pi_mod_sound_transpose)
          n += shift
        end

        n += args_h[:pitch].to_f

        if tuning_info = Thread.current.thread_variable_get(:sonic_pi_mod_sound_tuning)
          tuning_system, fundamental_sym = tuning_info
          if tuning_system != :equal
            n = @tuning.resolve_tuning(n, tuning_system, fundamental_sym)
          end
        end

        args_h[:note] = n
        trigger_inst synth_name, init_args_h.merge(args_h)
      end
      doc name:          :play,
          introduced:    Version.new(2,0,0),
          summary:       "Play current synth",
          doc:           "Play note with current synth. Accepts a set of standard options which include control of an amplitude envelope with `attack:`, `decay:`, `sustain:` and `release:` phases. These phases are triggered in order, so the duration of the sound is attack + decay + sustain + release times. The duration of the sound does not affect any other notes. Code continues executing whilst the sound is playing through its envelope phases.

Accepts optional args for modification of the synth being played. See each synth's documentation for synth-specific opts. See `use_synth` and `with_synth` for changing the current synth.

If note is `nil`, `:r` or `:rest`, play is ignored and treated as a rest.
    ",
          args:          [[:note, :symbol_or_number]],
          opts:          DEFAULT_PLAY_OPTS,
          accepts_block: false,
          intro_fn:       true,
          examples:      ["
play 50 # Plays note 50 on the current synth",

        "play 50, attack: 1 # Plays note 50 with a fade-in time of 1s",

        "play 62, pan: -1, release: 3 # Play note 62 in the left ear with a fade-out time of 3s." ]




      def play_pattern(notes, *args)
        notes.each{|note| play(note, *args) ; sleep 1 }
      end
      doc name:          :play_pattern,
          introduced:    Version.new(2,0,0),
          summary:       "Play pattern of notes",
          doc:           "Play list of notes with the current synth one after another with a sleep of 1

Accepts optional args for modification of the synth being played. See each synth's documentation for synth-specific opts. See use_synth and with_synth for changing the current synth.",
          args:          [[:notes, :list]],
          opts:          {},
          accepts_block: false,
          examples:      ["
play_pattern [40, 41, 42] # Same as:
                          #   play 40
                          #   sleep 1
                          #   play 41
                          #   sleep 1
                          #   play 42
",
        "play_pattern [:d3, :c1, :Eb5] # You can use keyword notes",

        "play_pattern [:d3, :c1, :Eb5], amp: 0.5, cutoff: 90 # Supports the same arguments as play:"]




      def play_pattern_timed(notes, times, *args)
        if times.is_a? Array
          notes.each_with_index{|note, idx| play(note, *args) ; sleep(times[idx % times.size])}
        else
          notes.each_with_index{|note, idx| play(note, *args) ; sleep times}
        end
      end
      doc name:          :play_pattern_timed,
          introduced:    Version.new(2,0,0),
          summary:       "Play pattern of notes with specific times",
          doc:           "Play each note in a list of notes one after another with specified times between them. The notes should be a list of MIDI numbers, symbols such as :E4 or chords such as chord(:A3, :major) - identical to the first parameter of the play function. The times should be a list of times between the notes in beats.

If the list of times is smaller than the number of gaps between notes, the list is repeated again. If the list of times is longer than the number of gaps between notes, then some of the times are ignored. See examples for more detail.

Accepts optional args for modification of the synth being played. See each synth's documentation for synth-specific opts. See `use_synth` and `with_synth` for changing the current synth.",
          args:          [[:notes, :list], [:times, :list_or_number]],
          opts:          DEFAULT_PLAY_OPTS,
          accepts_block: false,
          examples:      ["
play_pattern_timed [40, 42, 44, 46], [1, 2, 3]

# same as:

play 40
sleep 1
play 42
sleep 2
play 44
sleep 3
play 46",

        "play_pattern_timed [40, 42, 44, 46, 49], [1, 0.5]

# same as:

play 40
sleep 1
play 42
sleep 0.5
play 44
sleep 1
play 46
sleep 0.5
play 49",

        "play_pattern_timed [40, 42, 44, 46], [0.5]

# same as:

play 40
sleep 0.5
play 42
sleep 0.5
play 44
sleep 0.5
play 46",

        "play_pattern_timed [40, 42, 44], [1, 2, 3, 4, 5]

#same as:

play 40
sleep 1
play 42
sleep 2
play 44"]




      def play_chord(notes, *args)
        ensure_good_timing!

        shift = Thread.current.thread_variable_get(:sonic_pi_mod_sound_transpose) || 0
        shifted_notes = notes.map do |n|
          n = note(n) unless n.is_a? Numeric
          n + shift
        end
        synth_name = current_synth_name
        trigger_chord(synth_name, shifted_notes, args)
      end
      doc name:          :play_chord,
          introduced:    Version.new(2,0,0),
          summary:       "Play notes simultaneously",
          doc:           "Play a list of notes at the same time.

Accepts optional args for modification of the synth being played. See each synth's documentation for synth-specific opts. See `use_synth` and `with_synth` for changing the current synth.",
          args:          [[:notes, :list]],
          opts:          DEFAULT_PLAY_OPTS,
          accepts_block: false,
          examples:      ["
play_chord [40, 45, 47]

# same as:

play 40
play 45
play 47",

        "play_chord [40, 45, 47], amp: 0.5

# same as:

play 40, amp: 0.5
play 45, amp: 0.5
play 47, amp: 0.5",

        "play_chord chord(:e3, :minor)"]




      def use_merged_synth_defaults(*args, &block)
        raise "use_merged_synth_defaults does not work with a block. Perhaps you meant with_merged_synth_defaults" if block
        current_defs = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults)
        args_h = resolve_synth_opts_hash_or_array(args)
        merged_defs = (current_defs || {}).merge(args_h)
        Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, merged_defs
      end
      doc name:          :use_merged_synth_defaults,
          introduced:    Version.new(2,0,0),
          summary:       "Merge synth defaults",
          doc:           "Specify synth arg values to be used by any following call to play. Merges the specified values with any previous defaults, rather than replacing them.",
          args:          [],
          opts:          {},
          accepts_block: false,
          examples:      ["
play 50 #=> Plays note 50

use_merged_synth_defaults amp: 0.5
play 50 #=> Plays note 50 with amp 0.5

use_merged_synth_defaults cutoff: 80
play 50 #=> Plays note 50 with amp 0.5 and cutoff 80

use_merged_synth_defaults amp: 0.7
play 50 #=> Plays note 50 with amp 0.7 and cutoff 80
",

        "use_synth_defaults amp: 0.5, cutoff: 80, pan: -1
use_merged_synth_defaults amp: 0.7
play 50 #=> Plays note 50 with amp 0.7, cutoff 80 and pan -1"]





      def with_merged_synth_defaults(*args, &block)
        raise "with_merged_synth_defaults must be called with a do/end block" unless block
        current_defs = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults)

        args_h = resolve_synth_opts_hash_or_array(args)
        merged_defs = (current_defs || {}).merge(args_h)
        Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, merged_defs
        block.call
        Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, current_defs
      end
      doc name:           :with_merged_synth_defaults,
          introduced:     Version.new(2,0,0),
          summary:        "Block-level merge synth defaults",
          doc:            "Specify synth arg values to be used by any following call to play within the specified `do`/`end` block. Merges the specified values with any previous defaults, rather than replacing them. After the `do`/`end` block has completed, previous defaults (if any) are restored.",
          args:           [],
          opts:           {},
          accepts_block:  true,
          requires_block: true,
          examples:       ["
with_merged_synth_defaults amp: 0.5, pan: 1 do
  play 50 # => plays note 50 with amp 0.5 and pan 1
end",

        "play 50 #=> plays note 50
with_merged_synth_defaults amp: 0.5 do
  play 50 #=> plays note 50 with amp 0.5

  with_merged_synth_defaults pan: -1 do
    with_merged_synth_defaults amp: 0.7 do
      play 50 #=> plays note 50 with amp 0.7 and pan -1
    end
  end
  play 50 #=> plays note 50 with amp 0.5
end"]




      def use_synth_defaults(*args, &block)
        raise "use_synth_defaults does not work with a block. Perhaps you meant with_synth_defaults" if block
        args_h = resolve_synth_opts_hash_or_array(args)
        Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, args_h
      end
      doc name:          :use_synth_defaults,
          introduced:    Version.new(2,0,0),
          summary:       "Use new synth defaults",
          doc:           "Specify new default values to be used by all subsequent calls to `play`. Will remove and override any previous defaults.",
          args:          [],
          opts:          {},
          accepts_block: false,
          examples:      ["
play 50 # plays note 50 with default arguments

use_synth_defaults amp: 0.5, cutoff: 70

play 50 # plays note 50 with an amp of 0.5, cutoff of 70 and defaults for rest of args

use_synth_defaults cutoff: 90

play 50 # plays note 50 with a cutoff of 90 and defaults for rest of args - note that amp is no longer 0.5
"]




      def use_sample_defaults(*args, &block)
        raise "use_sample_defaults does not work with a block. Perhaps you meant with_sample_defaults" if block
        args_h = resolve_synth_opts_hash_or_array(args)
        Thread.current.thread_variable_set :sonic_pi_mod_sound_sample_defaults, args_h
      end
      doc name:          :use_sample_defaults,
          introduced:    Version.new(2,5,0),
          summary:       "Use new sample defaults",
          doc:           "Specify new default values to be used by all subsequent calls to `sample`. Will remove and override any previous defaults.",
          args:          [],
          opts:          {},
          accepts_block: false,
          examples:      ["
sample :loop_amen # plays amen break with default arguments

use_sample_defaults amp: 0.5, cutoff: 70

sample :loop_amen # plays amen break with an amp of 0.5, cutoff of 70 and defaults for rest of args

use_sample_defaults cutoff: 90

sample :loop_amen  # plays amen break with a cutoff of 90 and defaults for rest of args - note that amp is no longer 0.5
"]





      def with_sample_defaults(*args, &block)
        raise "with_sample_defaults must be called with a do/end block. Perhaps you meant use_sample_defaults" unless block
        current_defs = Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_defaults)
        args_h = resolve_synth_opts_hash_or_array(args)
        Thread.current.thread_variable_set :sonic_pi_mod_sound_sample_defaults, args_h
        block.call
        Thread.current.thread_variable_set :sonic_pi_mod_sound_sample_defaults, current_defs

      end
      doc name:           :with_sample_defaults,
          introduced:     Version.new(2,5,0),
          summary:        "Block-level use new sample defaults",
          doc:            "Specify new default values to be used by all subsequent calls to `sample` within the `do`/`end` block. After the `do`/`end` block has completed, the previous sampled defaults (if any) are restored. For the contents of the block, will remove and override any previous defaults.",
          args:           [],
          opts:           {},
          accepts_block:  false,
          requires_block: false,
          examples:       ["
sample :loop_amen # plays amen break with default arguments

use_sample_defaults amp: 0.5, cutoff: 70

sample :loop_amen # plays amen break with an amp of 0.5, cutoff of 70 and defaults for rest of args

with_sample_defaults cutoff: 90 do
  sample :loop_amen  # plays amen break with a cutoff of 90 and defaults for rest of args - note that amp is no longer 0.5
end

sample :loop_amen  # plays amen break with a cutoff of 70 and amp is 0.5 again as the previous defaults are restored."]




      def with_synth_defaults(*args, &block)
        raise "with_synth_defaults must be called with a do/end block" unless block
        current_defs = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults)

        args_h = resolve_synth_opts_hash_or_array(args)
        Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, args_h
        block.call
        Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, current_defs
      end
      doc name:           :with_synth_defaults,
          introduced:     Version.new(2,0,0),
          summary:        "Block-level use new synth defaults",
          doc:            "Specify new default values to be used by all calls to `play` within the `do`/`end` block. After the `do`/`end` block has completed the previous synth defaults (if any) are restored.",
          args:           [],
          opts:           {},
          accepts_block:  true,
          requires_block: true,
          examples:       ["
play 50 # plays note 50 with default arguments

use_synth_defaults amp: 0.5, pan: -1

play 50 # plays note 50 with an amp of 0.5, pan of -1 and defaults for rest of args

with_synth_defaults amp: 0.6, cutoff: 80 do
  play 50 # plays note 50 with an amp of 0.6, cutoff of 80 and defaults for rest of args (including pan)
end

play 60 # plays note 60 with an amp of 0.5, pan of -1 and defaults for rest of args
"]




      def use_fx(*args, &block)
        raise "use_fx isn't supported in this version of Sonic Pi. Perhaps you meant with_fx"
      end




      def with_afx(fx_name, *args, &block)
        in_thread do
          with_fx(fx_name, *args, &block)
        end
      end




      def with_fx(fx_name, *args, &block)
        raise "with_fx must be called with a do/end block" unless block
        raise "with_fx block must only accept 0 or 1 args" unless [0, 1].include?(block.arity)

        ## Munge args
        args_h = resolve_synth_opts_hash_or_array(args)
        args_h[:reps] = 1 unless args_h[:reps]

        ## Teach with_fx to do nothing if fx_name is :none
        if fx_name == :none
          if block.arity == 0
            return args_h[:reps].times do
              block.call
            end
          else
            return args_h[:reps].times do
              block.call(@blank_node)
            end
          end
        end
        fx_synth_name = "fx_#{fx_name}"

        info = SynthInfo.get_info(fx_synth_name)
        raise "Unknown fx #{fx_name.inspect}" unless info

        start_subthreads = []
        end_subthreads = []

        fxt = Thread.current
        p = Promise.new
        gc_completed = Promise.new

        # These will be assigned later...
        fx_synth = BlankNode.new
        new_bus = nil
        current_bus = current_out_bus
        tracker = nil
        fx_group = nil
        job_id = Thread.current.thread_variable_get :sonic_pi_spider_job_id
        block_res = nil

        __no_kill_block do

          ## We're in a no_kill block, so the user can't randomly kill
          ## the current thread. That means it's safe to set up the
          ## synth trackers, create the fx synth and busses and modify
          ## the thread local to make sure new synth triggers in this
          ## thread output to this fx synth. We can also create a gc
          ## thread to wait for the current thread to either exit
          ## correctly or to die and to handle things appropriately.


          ## Create a new bus for this fx chain
          begin
            new_bus = @mod_sound_studio.new_fx_bus
          rescue AllocationError
            __delayed_serious_warning "All busses allocated - unable to honour FX"
            if block.arity == 0
              return args_h[:reps].times do
                block.call
              end
            else
              return args_h[:reps].times do
                block.call(@blank_node)
              end
            end
          end

          args_h["in_bus"] = new_bus
          args_h = normalise_and_resolve_synth_args(args_h, info)


          # Setup trackers
          current_trackers = Thread.current.thread_variable_get(:sonic_pi_mod_sound_trackers) || Set.new

          # Create new group for this FX - this is to enable the FX to be triggered at logical time
          # whilst ensuring it is in the correct position in the scsynth node tree.
          fx_group = @mod_sound_studio.new_group(:head, current_fx_main_group, "Run-#{job_id}-#{fx_name}")

          ## Create a 'GC' thread to safely handle completion of the FX
          ## block (or the case that the thread dies) and to clean up
          ## everything appropriately (i.e. ensure the FX synth has
          ## been killed).
          gc = Thread.new do

            Thread.current.thread_variable_set(:sonic_pi_thread_group, :gc)
            Thread.current.priority = -10
            ## Need to block until either the thread died (which will be
            ## if the job was stopped whilst this fx block was being
            ## executed or if the fx block has completed.
            fx_completed = Promise.new

            t1 = Thread.new do
              Thread.current.thread_variable_set(:sonic_pi_thread_group, :gc_parent_join)
              Thread.current.priority = -10
              fxt.join
              ## Parent thread died - user must have stopped
              fx_completed.deliver! :thread_joined, false
            end

            t2 = Thread.new do
              Thread.current.thread_variable_set(:sonic_pi_thread_group, :gc_fx_block_join)
              Thread.current.priority = -10
              p.get
              ## FX block completed
              fx_completed.deliver! :fx_block_completed, false
            end

            ## Block!
            fx_completed.get
            ## Clean up blocking alert threads (one of them already
            ## completed, but kill both for completeness)
            t1.kill
            t2.kill

            ## Remove synth tracker
            current_trackers.delete tracker

            ## Get a list of subthreads created by this fx block and create
            ## a new thread which will wait for them all to finish before
            ## killing the fx synth node and free its bus
            fxt.thread_variable_get(:sonic_pi_spider_subthread_mutex).synchronize do
              end_subthreads = fxt.thread_variable_get(:sonic_pi_spider_subthreads).to_a
            end

            new_subthreads = (end_subthreads - start_subthreads)

            Thread.new do
              Thread.current.thread_variable_set(:sonic_pi_thread_group, :gc_kill_fx_synth)
              Thread.current.priority = -10
              kill_delay = args_h[:kill_delay] || info.kill_delay(args_h)
              new_subthreads.each do |st|
                join_thread_and_subthreads(st)
              end
              ## Sleep for half a second to ensure that any synths
              ## triggered in the threads joined above get chance to
              ## asynchronously communicate their existence to the
              ## tracker. (This happens in a Node#on_started handler)
              Kernel.sleep 0.5 + @mod_sound_studio.sched_ahead_time
              tracker.block_until_finished
              Kernel.sleep(kill_delay)
              fx_group.kill(true)
            end

            gc_completed.deliver! true
          end ## end gc collection thread definition

          ## Trigger new fx synth (placing it in the fx group) and
          ## piping the in and out busses correctly
          t_minus_delta = info.trigger_with_logical_clock? == :t_minus_delta
          fx_synth = trigger_fx(fx_synth_name, args_h, info, new_bus, fx_group, !info.trigger_with_logical_clock?, t_minus_delta)

          ## Create a synth tracker and stick it in a thread local
          tracker = SynthTracker.new

          ## Get list of current subthreads. We'll need this later to
          ## determine which threads were created as a result of the fx
          ## block so we can wait for them all to finish before freeing
          ## the busses and fx synth.
          Thread.current.thread_variable_get(:sonic_pi_spider_subthread_mutex).synchronize do
            start_subthreads = Thread.current.thread_variable_get(:sonic_pi_spider_subthreads).to_a
          end

          ## Set this thread's out bus to pipe audio into the new fx synth node
          Thread.current.thread_variable_set(:sonic_pi_mod_sound_synth_out_bus, new_bus)

        end #end don't kill sync



        ## Now actually execute the fx block. Pass the fx synth in as a
        ## parameter if the block was defined with a param.
        block_exception = nil
        fx_execute_t = in_thread do
          Thread.current.thread_variable_set(:sonic_pi_spider_delayed_blocks, fxt.thread_variable_get(:sonic_pi_spider_delayed_blocks))
          Thread.current.thread_variable_set(:sonic_pi_spider_delayed_messages, fxt.thread_variable_get(:sonic_pi_spider_delayed_messages))
          Thread.current.thread_variable_set(:sonic_pi_spider_random_gen_idx, fxt.thread_variable_get(:sonic_pi_spider_random_gen_idx))
          Thread.current.thread_variable_set(:sonic_pi_core_thread_local_counters, fxt.thread_variable_get(:sonic_pi_core_thread_local_counters))

          new_trackers = [tracker]
          (Thread.current.thread_variable_get(:sonic_pi_mod_sound_trackers) || []).each do |tr|
            new_trackers << tr
          end
          Thread.current.thread_variable_set(:sonic_pi_mod_sound_trackers, new_trackers)
          cur_fx_group = Thread.current.thread_variable_get(:sonic_pi_mod_sound_fx_group)
          Thread.current.thread_variable_set(:sonic_pi_mod_sound_fx_group, fx_group)
          begin
            if block.arity == 0
              args_h[:reps].times do
                block_res = block.call
              end
            else
              args_h[:reps].times do
                block_res = block.call(fx_synth)
              end
            end
          rescue => e
            block_exception = e
          ensure
            ## Ensure that p's promise is delivered - thus kicking off
            ## the gc thread.
            p.deliver! true
            ## Reset out bus to value prior to this with_fx block
            fxt.thread_variable_set(:sonic_pi_mod_sound_synth_out_bus, current_bus)
            Thread.current.thread_variable_set(:sonic_pi_mod_sound_fx_group, cur_fx_group)
          end
        end

        # Join thread used to execute block. Then transfer virtual
        # timestamp back to this thread.
        fx_execute_t.join
        raise block_exception if block_exception
        Thread.current.thread_variable_set(:sonic_pi_spider_delayed_blocks, fx_execute_t.thread_variable_get(:sonic_pi_spider_delayed_blocks))
        Thread.current.thread_variable_set(:sonic_pi_spider_delayed_messages, fx_execute_t.thread_variable_get(:sonic_pi_spider_delayed_messages))
        Thread.current.thread_variable_set(:sonic_pi_spider_time, fx_execute_t.thread_variable_get(:sonic_pi_spider_time))
        Thread.current.thread_variable_set(:sonic_pi_core_thread_local_counters, fx_execute_t.thread_variable_get(:sonic_pi_core_thread_local_counters))

        Thread.current.thread_variable_set(:sonic_pi_spider_random_gen_idx, fx_execute_t.thread_variable_get(:sonic_pi_spider_random_gen_idx))

        ## Ensure the synced detection mechanism comes back out of
        ## with_fx blocks so syncs can be within with_fx blocks within
        ## live_loops without tripping the live_loop no sleep detector
        Thread.current.thread_variable_set(:sonic_pi_spider_synced, fx_execute_t.thread_variable_get(:sonic_pi_spider_synced))

        # Wait for gc thread to complete. Once the gc thread has
        # completed, the tracker has been successfully removed, and all
        # the block threads have been determined. The gc thread has
        # spawned a new thread joining on those and waiting for all
        # remaining synths to complete and can be left to work in the
        # background...
        gc_completed.get

        # return result of block
        block_res
      end
      doc name:           :with_fx,
          introduced:     Version.new(2,0,0),
          summary:        "Use Studio FX",
          doc:            "This applies the named effect (FX) to everything within a given `do`/`end` block. Effects may take extra parameters to modify their behaviour. See FX help for parameter details.

For advanced control, it is also possible to modify the parameters of an effect within the body of the block. If you define the block with a single argument, the argument becomes a reference to the current effect and can be used to control its parameters (see examples).",
          args:           [[:fx_name, :symbol]],
          opts:           {reps: "Number of times to repeat the block in an iteration.",
            kill_delay: "Amount of time to wait after all synths triggered by the block have completed before stopping and freeing the effect synthesiser." },
          accepts_block:  true,
          requires_block: true,
          intro_fn:       true,
          examples:      ["
# Basic usage
with_fx :distortion do # Use the distortion effect with default parameters
  play 50 # => plays note 50 with distortion
  sleep 1
  sample :loop_amen # => plays the loop_amen sample with distortion too
end",


        "# Specify effect parameters
with_fx :level, amp: 0.3 do # Use the level effect with the amp parameter set to 0.3
  play 50
  sleep 1
  sample :loop_amen
end",

        "
# Controlling the effect parameters within the block
with_fx :reverb, mix: 0.1 do |fx|
  # here we set the reverb level quite low to start with (0.1)
  # and we can change it later by using the 'fx' reference we've set up

  play 60 # plays note 60 with a little bit of reverb
  sleep 2

  control fx, mix: 0.5 # change the parameters of the effect to add more reverb
  play 60 # again note 60 but with more reverb
  sleep 2

  control fx, mix: 1 # change the parameters of the effect to add more reverb
  play 60 # plays note 60 with loads of reverb
  sleep 2
end",

        "
# Repeat the block 16 times internally
with_fx :reverb, reps: 16 do
  play (scale :e3, :minor_pentatonic), release: 0.1
  sleep 0.125
end

# The above is a shorthand for this:
with_fx :reverb do
  16.times do
    play (scale :e3, :minor_pentatonic), release: 0.1
    sleep 0.125
  end
end
"
      ]




      def use_sample_pack(pack, &block)
        raise "use_sample_pack does not work with a block. Perhaps you meant with_sample_pack" if block
        if pack == :default
          pack = samples_path + "/"
        else
          pack = "#{pack}/" if File.directory?(pack)
        end

        Thread.current.thread_variable_set(:sonic_pi_mod_sound_sample_path, pack)
      end
      doc name:          :use_sample_pack,
          introduced:    Version.new(2,0,0),
          summary:       "Use sample pack",
          doc:           "Given a path to a folder of samples on your filesystem, this method makes any `.wav`, `.wave`, `.aif` or `.aiff` files in that folder available as samples. Consider using `use_sample_pack_as` when using multiple sample packs. Use `use_sample_pack :default` To revert back to the default built-in samples.",
          args:          [[:pack_path, :string]],
          opts:          nil,
          accepts_block: false,
          examples:
        ["
use_sample_pack '/home/yourname/path/to/sample/dir'
sample :foo  #=> plays /home/yourname/path/to/sample/dir/foo.{wav|wave|aif|aiff}
             #   where {wav|wave|aif|aiff} means one of wav, wave aif or aiff.
sample :bd_haus #=> will not work unless there's a sample in '/home/yourname/path/to/sample/dir'
                #   called bd_haus.{wav|wave|aif|aiff}
use_sample_pack :default
sample :bd_haus #=> will play the built-in bd_haus.wav sample" ]




      def use_sample_pack_as(pack, pack_alias, &block)
        raise "use_sample_pack_as does not work with a block. Perhaps you meant with_sample_pack_as" if block
        pack = "#{pack}/" if File.directory?(pack)
        aliases = Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_aliases) || Hamster.hash
        new_aliases = aliases.put pack_alias.to_s, pack
        Thread.current.thread_variable_set(:sonic_pi_mod_sound_sample_aliases, new_aliases)
      end
      doc name:          :use_sample_pack_as,
          introduced:    Version.new(2,0,0),
          summary:       "Use sample pack alias",
          doc:           "Similar to `use_sample_pack` except you can assign prefix aliases for samples. This lets you 'namespace' your sounds so that they don't clash, even if they have the same filename.",
          args:          [[:path, :string], [:alias, :string]],
          opts:          nil,
          accepts_block: false,
          examples:      ["
# let's say you have two folders of your own sample files,
# and they both contain a file named 'bass.wav'
use_sample_pack_as '/home/yourname/my/cool/samples/guitar', :my_guitars
use_sample_pack_as '/home/yourname/my/cool/samples/drums', :my_drums

# You can now play both the 'bass.wav' samples, as they've had the symbol stuck on the front
sample :my_guitars__bass    #=> plays '/home/yourname/my/cool/samples/guitar/bass.wav'
sample :my_drums__bass  #=> plays '/home/yourname/my/cool/samples/drums/bass.wav'"]




      def with_sample_pack(pack, &block)
        raise "with_sample_pack requires a block. Perhaps you meant use_sample_pack" unless block
        if pack == :default
          # allow user to reset sample pack with the :default keyword
          pack = samples_path
        else
          # ensure directories have trailing /
          pack = "#{pack}/" if File.directory?(pack)
        end
        current = Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_path)
        Thread.current.thread_variable_set(:sonic_pi_mod_sound_sample_path, pack)
        block.call
        Thread.current.thread_variable_set(:sonic_pi_mod_sound_sample_path, current)
      end
      doc name:           :with_sample_pack,
          introduced:     Version.new(2,0,0),
          summary:        "Block-level use sample pack",
          doc:            "Given a path to a folder of samples on your filesystem, this method makes any `.wav`, `.wave`, `.aif`, or `.aiff` files in that folder available as samples inside the given block. Consider using `with_sample_pack_as` when using multiple sample packs.",
          args:           [[:pack_path, :string]],
          opts:           nil,
          accepts_block:  true,
          requires_block: true,
          examples:       ["
with_sample_pack '/path/to/sample/dir' do
  sample :foo  #=> plays /path/to/sample/dir/foo.{wav|wave|aif|aiff}
end"]




      def with_sample_pack_as(pack, name, &block)
        raise "with_sample_pack_as requires a do/end block. Perhaps you meant use_sample_pack_as" unless block
        pack = "#{pack}/" if File.directory?(pack)
        current = Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_aliases)
        aliases = current || Hamster.hash
        new_aliases = aliases.put name.to_s, pack
        Thread.current.thread_variable_set(:sonic_pi_mod_sound_sample_aliases, new_aliases)
        block.call
        Thread.current.thread_variable_set(:sonic_pi_mod_sound_sample_aliases, current)
      end
      doc name:           :with_sample_pack_as,
          introduced:     Version.new(2,0,0),
          summary:        "Block-level use sample pack alias",
          doc:            "Similar to `with_sample_pack` except you can assign prefix aliases for samples. This lets you 'namespace' your sounds so that they don't clash, even if they have the same filename.",
          args:           [[:pack_path, :string]],
          opts:           nil,
          accepts_block:  true,
          requires_block: true,
          examples:       ["
with_sample_pack_as '/home/yourname/path/to/sample/dir', :my_samples do
  # The foo sample is now available, with a prefix of 'my_samples'
  sample :my_samples__foo  #=> plays /home/yourname/path/to/sample/dir/foo.{wav|wave|aif|aiff}
end"]




      def current_synth
        current_synth_name
      end
      doc name:          :current_synth,
          introduced:    Version.new(2,0,0),
          summary:       "Get current synth",
          doc:           "Returns the current synth name.",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      ["
puts current_synth # Print out the current synth name"]




      def current_sample_pack
        Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_path)
      end
      doc name:          :current_sample_pack,
          introduced:    Version.new(2,0,0),
          summary:       "Get current sample pack",
          doc:           "Returns the current sample pack.",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      ["
puts current_sample_pack # Print out the current sample pack"]




      def current_sample_pack_aliases
        Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_aliases)
      end
      doc name:          :current_sample_pack_aliases,
          introduced:    Version.new(2,0,0),
          summary:       "Get current sample pack aliases",
          doc:           "Returns a map containing the current sample pack aliases.",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      ["
puts current_sample_pack_aliases # Print out the current sample pack aliases"]




      def current_synth_defaults
        Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults)
      end
      doc name:          :current_synth_defaults,
          introduced:    Version.new(2,0,0),
          summary:       "Get current synth defaults",
          doc:           "Returns the current synth defaults. This is a map of synth arg names to either values or functions.",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      ["
use_synth_defaults amp: 0.5, cutoff: 80
play 50 # Plays note 50 with amp 0.5 and cutoff 80
puts current_synth_defaults #=> Prints {amp: 0.5, cutoff: 80}"]




      def current_sample_defaults
        Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_defaults)
      end
      doc name:          :current_sample_defaults,
          introduced:    Version.new(2,5,0),
          summary:       "Get current sample defaults",
          doc:           "Returns the current sample defaults. This is a map of synth arg names to either values or functions.",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      ["
use_sample_defaults amp: 0.5, cutoff: 80
sample :loop_amen # Plays amen break with amp 0.5 and cutoff 80
puts current_sample_defaults #=> Prints {amp: 0.5, cutoff: 80}"]




      def current_sched_ahead_time
        @mod_sound_studio.sched_ahead_time
      end
      doc name:          :current_sched_ahead_time,
          introduced:    Version.new(2,0,0),
          summary:       "Get current sched ahead time",
          doc:           "Returns the current schedule ahead time.",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      ["
set_sched_ahead_time! 0.5
puts current_sched_ahead_time # Prints 0.5"]




      def current_volume
        @mod_sound_studio.volume
      end
      doc name:          :current_volume,
          introduced:    Version.new(2,0,0),
          summary:       "Get current volume",
          doc:           "Returns the current volume.",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      ["
puts current_volume # Print out the current volume",
        "set_volume! 2
puts current_volume #=> 2"]




      def current_transpose
        Thread.current.thread_variable_get(:sonic_pi_mod_sound_transpose)
      end
      doc name:          :current_transpose,
          introduced:    Version.new(2,0,0),
          summary:       "Get current transposition",
          doc:           "Returns the current transpose value.",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      ["
puts current_transpose # Print out the current transpose value"]




      def current_debug
        Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_silent)
      end
      doc name:          :current_debug,
          introduced:    Version.new(2,0,0),
          summary:       "Get current debug status",
          doc:           "Returns the current debug setting (`true` or `false`).",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      ["
puts current_debug # Print out the current debug setting"]




      def current_arg_checks
        Thread.current.thread_variable_get(:sonic_pi_mod_sound_check_synth_args)
      end
      doc name:          :current_arg_checks,
          introduced:    Version.new(2,0,0),
          summary:       "Get current arg checking status",
          doc:           "Returns the current arg checking setting (`true` or `false`).",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      ["
puts current_arg_checks # Print out the current arg check setting"]




      def set_volume!(vol)
        max_vol = 5
        if (vol > max_vol)
          new_vol = max_vol
        elsif (vol < 0)
          new_vol = 0
        else
          new_vol = vol
        end
        @mod_sound_studio.volume = new_vol
        __info "Volume set to: #{new_vol}"
      end
      doc name:          :set_volume!,
          introduced:    Version.new(2,0,0),
          summary:       "Set Volume globally",
          doc:           "Set the main system volume to `vol`. Accepts a value between `0` and `5` inclusive. Vols greater or smaller than the allowed values are trimmed to keep them within range. Default is `1`.",
          args:          [[:vol, :number]],
          opts:          nil,
          accepts_block: false,
          modifies_env: true,
          examples:      ["
set_volume! 2 # Set the main system volume to 2",

        "set_volume! -1 # Out of range, so sets main system volume to 0",

        "set_volume! 7 # Out of range, so sets main system volume to 5"
      ]




      def sample_loaded?(path)
        case path
        when Symbol
          full_path = resolve_sample_symbol_path(path)
          return @mod_sound_studio.sample_loaded?(full_path)
        when String
          path = File.expand_path(path)
          return @mod_sound_studio.sample_loaded?(path)
        else
          raise "Unknown sample description: #{path}"
        end
      end
      doc name:          :sample_loaded?,
          introduced:    Version.new(2,2,0),
          summary:       "Test if sample was pre-loaded",
          doc:           "Given a path to a `.wav`, `.wave`, `.aif` or `.aiff` file, returns `true` if the sample has already been loaded.",
          args:          [[:path, :string]],
          opts:          nil,
          accepts_block: false,
          examples:      ["
load_sample :elec_blip # :elec_blip is now loaded and ready to play as a sample
puts sample_loaded? :elec_blip # prints true because it has been pre-loaded
puts sample_loaded? :misc_burp # prints false because it has not been loaded"]




      def load_sample(path)
        case path
        when Symbol
          full_path = resolve_sample_symbol_path(path)
          info, cached = @mod_sound_studio.load_sample(full_path)
          __info "Loaded sample :#{path}" unless cached
          return info
        when String
          raise "Attempted to load sample with an empty string as path" if path.empty?
          path = File.expand_path(path)
          if File.exists?(path)
            info, cached = @mod_sound_studio.load_sample(path)
            __info "Loaded sample #{path.inspect}" unless cached
            return info
          else
            raise "No sample exists with path #{path}"
          end
        else
          raise "Unknown sample description: #{path}. Expected a symbol such as :loop_amen or a string containing a path."
        end
      end
      doc name:          :load_sample,
          introduced:    Version.new(2,0,0),
          summary:       "Pre-load sample",
          doc:           "Given a path to a `.wav`, `.wave`, `.aif` or `.aiff` file, this loads the file and makes it available as a sample. See `load_samples` for loading multiple samples in one go.",
          args:          [[:path, :string]],
          opts:          nil,
          accepts_block: false,
          examples:      ["
load_sample :elec_blip # :elec_blip is now loaded and ready to play as a sample
sample :elec_blip # No delay takes place when attempting to trigger it"]




      def load_samples(*paths)
        paths.each do |p|
          if p.kind_of?(Array)
            load_samples *p
          else
            load_sample p
          end
        end
      end
      doc name:          :load_samples,
          introduced:    Version.new(2,0,0),
          summary:       "Pre-load samples",
          doc:           "Given an array of paths to `.wav`, `.wave`, `.aif` or `.aiff` files, loads them all into memory so that they may be played with via sample with no delay. See `load_sample`.",
          args:          [[:paths, :list]],
          opts:          nil,
          accepts_block: false,
          examples:      ["
sample :ambi_choir # This has to first load the sample before it can play it which may
                   # cause unwanted delay.

load_samples [:elec_plip, :elec_blip] # Let's load some samples in advance of using them
sample :elec_plip                     # When we play :elec_plip, there is no extra delay
                                      # as it has already been loaded.",

        "
load_samples :elec_plip, :elec_blip # You may omit the square brackets, and
                                    # simply list all samples you wish to load
sample :elec_blip                   # Before playing them.",
        "
load_samples [\"/home/pi/samples/foo.wav\"] # You may also load full paths to samples.
sample \"/home/pi/sample/foo.wav\"          # And then trigger them with no more loading."]




      def sample_info(path)
        load_sample(path)
      end
      doc name:          :sample_info,
          introduced:    Version.new(2,0,0),
          summary:       "Get sample information",
          doc:           "Alias for the `load_sample` method. Loads sample if necessary and returns sample information.",
          args:          [[:path, :string]],
          opts:          nil,
          accepts_block: false,
          examples:      ["see load_sample"]




      def sample_buffer(path)
        load_sample(path)
      end
      doc name:          :sample_buffer,
          introduced:    Version.new(2,0,0),
          summary:       "Get sample data",
          doc:           "Alias for the `load_sample` method. Loads sample if necessary and returns buffer information.",
          args:          [[:path, :string]],
          opts:          nil,
          accepts_block: false,
          examples:      ["see load_sample"]




      def sample_duration(path, *args)
        dur = load_sample(path).duration
        args_h = resolve_synth_opts_hash_or_array(args)
        args_h[:rate] = 1 unless args_h[:rate]
        start = args_h[:start] || 0
        start = [1, [0, start].max].min
        finish = args_h[:finish] || 1
        finish = [1, [0, finish].max].min

        # adjust for both beat and pitch stretching
        # (which are BPM dependent)
        if args_h[:beat_stretch]
          beat_stretch = args_h[:beat_stretch].to_f
          beat_rate_mod = (1.0 / beat_stretch) * args_h[:rate] * (current_bpm / (60.0 / dur))
          args_h[:rate] = args_h[:rate] * beat_rate_mod
        end

        if args_h[:pitch_stretch]
          pitch_stretch = args_h[:pitch_stretch].to_f
          pitch_rate_mod = (1.0 / pitch_stretch) * args_h[:rate] * (current_bpm / (60.0 / dur))
          args_h[:rate] = args_h[:rate] * pitch_rate_mod
        end


        if finish > start
          len = finish - start
        else
          len = start - finish
        end
        real_dur = dur * 1.0/(args_h[:rate].abs) * len

        if args_h.has_key?(:sustain)
          attack = [0, args_h[:attack].to_f].max
          decay = [0, args_h[:decay].to_f].max
          release = [0, args_h[:release].to_f].max
          real_dur = [attack + decay + release, real_dur].min
        end

        if Thread.current.thread_variable_get(:sonic_pi_spider_arg_bpm_scaling)
          return real_dur.to_f / Thread.current.thread_variable_get(:sonic_pi_spider_sleep_mul)
        else
          return real_dur
        end
      end
      doc name:          :sample_duration,
      introduced:    Version.new(2,0,0),
      summary:       "Get sample duration in beats",
      doc:           "Given the name of a loaded sample, or a path to a `.wav`, `.wave`, `.aif` or `.aiff` file this returns the length of time in beats that the sample would play for. It's useful when looping samples to make sure there are no gaps - see the examples. You may pass a rate opt which it will use to scale the returned time to match the duration at that rate. The time returned is scaled to the current bpm.",
      args:          [[:path, :string]],
      opts:          {:rate => "Rate modifier. For example, doubling the rate will halve the duration.",
        :start => "Start position of sample playback as a value from 0 to 1",
        :finish => "Finish position of sample playback as a value from 0 to 1",
        :attack => "Duration of the attack phase of the envelope.",
        :decay => "Duration of the decay phase of the envelope.",
        :sustain => "Duration of the sustain phase of the envelope.",
        :release => "Duration of the release phase of the envelope."},

      accepts_block: false,
      examples:      ["
loop do   # Using sample_duration here means the loop plays back without any gaps or breaks
  sample :loop_amen # Play amen break
  sleep sample_duration(:loop_amen) # sleep for duration of amen break
end",

        "loop do  # You can also use rate if you want to keep a seamless loop whilst adjusting the speed
  sample :loop_amen, rate: 0.75
  sleep sample_duration(:loop_amen, rate: 0.75)
end",

        "
loop do
  sample :loop_amen, rate: -1 # Works for negative rates too
  sleep sample_duration :loop_amen, rate: -1
end ",

        "
use_sample_bpm :loop_amen
puts sample_duration(:loop_amen) #=> 1
"
      ]




      def sample(path, *args_a_or_h)
        return if path == nil
        ensure_good_timing!
        buf_info = load_sample(path)
        args_h = resolve_synth_opts_hash_or_array(args_a_or_h)
        stretch_duration = args_h[:beat_stretch]
        if stretch_duration
          raise "beat_stretch: opt needs to be a positive number. Got: #{stretch_duration.inspect}" unless stretch_duration.is_a?(Numeric) && stretch_duration > 0
          stretch_duration = stretch_duration.to_f
          rate = args_h[:rate] || 1
          dur = load_sample(path).duration
          args_h[:rate] = (1.0 / stretch_duration) * rate * (current_bpm / (60.0 / dur))
        end

        pitch_stretch_duration = args_h[:pitch_stretch]
        if pitch_stretch_duration
          raise "pitch_stretch: opt needs to be a positive number. Got: #{pitch_stretch_duration.inspect}" unless pitch_stretch_duration.is_a?(Numeric) && pitch_stretch_duration > 0
          pitch_stretch_duration = pitch_stretch_duration.to_f
          rate = args_h[:rate] || 1
          dur = load_sample(path).duration
          new_rate = (1.0 / pitch_stretch_duration) * rate * (current_bpm / (60.0 / dur))
          pitch_shift = ratio_to_pitch(new_rate)
          args_h[:rate] = new_rate * (args_h[:rate] || 1)
          args_h[:pitch] = args_h[:pitch].to_f - pitch_shift
        end

        rate_pitch = args_h[:rpitch]
        if rate_pitch
          new_rate = pitch_to_ratio(rate_pitch.to_f)
          args_h[:rate] = new_rate * (args_h[:rate] || 1)
        end

        trigger_sampler path, buf_info.id, buf_info.num_chans, args_h
      end
      doc name:          :sample,
          introduced:    Version.new(2,0,0),
          summary:       "Trigger sample",
          doc:           "This is the main method for playing back recorded sound files (samples). Sonic Pi comes with lots of great samples included (see the section under help) but you can also load and play `.wav`, `.wave`, `.aif` or `.aiff` files from anywhere on your computer too. The `rate:` parameter affects both the speed and the pitch of the playback. See the examples for details. Check out the `use_sample_pack` and `use_sample_pack_as` fns for details on making it easy to work with a whole folder of your own sample files. Note, that on the first trigger of a sample, Sonic Pi has to load the sample which takes some time and may cause timing issues. To preload the samples you wish to work with consider using `load_sample` or `load_samples`.",
          args:          [[:name_or_path, :symbol_or_string]],
          opts:          {:rate          => "Rate with which to play back the sample. Higher rates mean an increase in pitch and a decrease in duration. Default is 1.",
                          :beat_stretch  => "Stretch (or shrink) the sample to last for exactly the specified number of beats. Please note - this does *not* keep the pitch constant and is essentially the same as modifying the rate directly.",
                          :pitch_stretch => "Stretch (or shrink) the sample to last for exactly the specified number of beats. This attempts to keep the pitch constant using the pitch: opt. Note, it's very likely you'll need to experiment with the window_size: pitch_dis: and time_dis: opts depending on the sample and the amount you'd like to stretch/shrink from original size.",
                          :attack        => "Time to reach full volume. Default is 0",
                          :sustain       => "Time to stay at full volume. Default is to stretch to length of sample (minus attack and release times).",
                          :release       => "Time (from the end of the sample) to go from full amplitude to 0. Default is 0",
                          :start         => "Position in sample as a fraction between 0 and 1 to start playback. Default is 0.",
                          :finish        => "Position in sample as a fraction between 0 and 1 to end playback. Default is 1.",
                          :pan           => "Stereo position of audio. -1 is left ear only, 1 is right ear only, and values in between position the sound accordingly. Default is 0",
                          :amp           => "Amplitude of playback",
                          :norm          => "Normalise the audio (make quieter parts of the sample louder and louder parts quieter) - this is similar to the normaliser FX. This may emphasise any clicks caused by clipping.",
                          :cutoff        => "Cutoff value of the built-in low pass filter (lpf) in MIDI notes. Unless specified, the lpf is *not* added to the signal chain.",
                          :res           => "Cutoff-specific opt. Only honoured if cutoff: is specified. Filter resonance as a value between 0 and 1. Large amounts of resonance (a res: near 1) can create a whistling sound around the cutoff frequency. Smaller values produce less resonance.",
                          :rpitch        => "Rate modified pitch. Multiplies the rate by the appropriate ratio to shift up or down the specified amount in MIDI notes. Please note - this does *not* keep the duration and rhythmical rate constant and ie essentially the same as modifying the rate directly.",
                          :pitch         => "Pitch adjustment in semitones. 1 is up a semitone, 12 is up an octave, -12 is down an octave etc. Maximum upper limit of 24 (up 2 octaves). Lower limit of -72 (down 6 octaves). Decimal numbers can be used for fine tuning.",
                          :window_size   => "Pitch shift-specific opt - only honoured if the pitch: opt is used. Pitch shift works by chopping the input into tiny slices, then playing these slices at a higher or lower rate. If we make the slices small enough and overlap them, it sounds like the original sound with the pitch changed. The window_size is the length of the slices and is measured in seconds. It needs to be around 0.2 (200ms) or greater for pitched sounds like guitar or bass, and needs to be around 0.02 (20ms) or lower for percussive sounds like drum loops. You can experiment with this to get the best sound for your input.",
                          :pitch_dis     => "Pitch shift-specific opt - only honoured if the pitch: opt is used. Pitch dispersion - how much random variation in pitch to add. Using a low value like 0.001 can help to \"soften up\" the metallic sounds, especially on drum loops. To be really technical, pitch_dispersion is the maximum random deviation of the pitch from the pitch ratio (which is set by the pitch param)",
                          :time_dis      => "Pitch shift-specific opt - only honoured if the pitch: opt is used. Time dispersion - how much random delay before playing each grain (measured in seconds). Again, low values here like 0.001 can help to soften up metallic sounds introduced by the effect. Large values are also fun as they can make soundscapes and textures from the input, although you will most likely lose the rhythm of the original. NB - This won't have an effect if it's larger than window_size.",
                          :slide         => "Default slide time in beats for all slide opts. Individually specified slide opts will override this value" },
          accepts_block: false,
          intro_fn:       true,


          examples:      ["
sample :perc_bell # plays one of Sonic Pi's built in samples",
        "sample '/home/yourname/path/to/a/sample.wav' # plays a wav|wave|aif|aiff file from your local filesystem",
        "# Let's play with the rate parameter
# play one of the included samples
sample :loop_amen
sleep sample_duration(:loop_amen) # this sleeps for exactly the length of the sample

# Setting a rate of 0.5 will cause the sample to
#   a) play half as fast
#   b) play an octave down in pitch
#
# Listen:
sample :loop_amen, rate: 0.5
sleep sample_duration(:loop_amen, rate: 0.5)

# Setting a really low number means the sample takes
# a very long time to finish! Also it sounds very
# different to the original sound
sample :loop_amen, rate: 0.05
sleep sample_duration(:loop_amen, rate: 0.05)",
        "# Setting a really negative number can be lots of fun
# It plays the sample backwards!
sample :loop_amen, rate: -1
sleep sample_duration(:loop_amen, rate: 1)  # there's no need to give sample_duration a negative number though

                                             # Using a rate of -0.5 is just like using the positive 0.5
                                             # (lower in pitch and slower) except backwards
sample :loop_amen, rate: -0.5
sleep sample_duration(:loop_amen, rate: 0.5) # there's no need to give sample_duration a negative number though",
        "# BE CAREFUL
# Don't set the rate to 0 though because it will get stuck
# and won't make any sound at all!
# We can see that the following would take Infinity seconds to finish
puts sample_duration(:loop_amen, rate: 0)",
        "# Just like the play method, we can assign our sample player
# to a variable and control the rate parameter whilst it's playing.
#
# The following example sounds a bit like a vinyl speeding up
# Note, this technique only works when you don't use envelope or start/finish opts.
s = sample :loop_amen_full, rate: 0.05
sleep 1
control(s, rate: 0.2)
sleep 1
control(s, rate: 0.4)
sleep 1
control(s, rate: 0.6)
sleep 1
control(s, rate: 0.8)
sleep 1
control(s, rate: 1)",
        "
# Using the :start and :finish parameters you can play a section of the sample.
# The default start is 0 and the default finish is 1
sample :loop_amen, start: 0.5, finish: 1 # play the last half of a sample",
        "
# You can also play part of any sample backwards by using a start value that's
# higher than the finish
sample :loop_amen, start: 1, finish: 0.5 # play the last half backwards"]




      def status
        @mod_sound_studio.status
      end
      doc name:          :status,
          introduced:    Version.new(2,0,0),
          summary:       "Get server status",
          doc:           "This returns a Hash of information about the synthesis environment. Mostly used for debugging purposes.",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      ["
puts status # Returns something similar to:
            # {
            #   :ugens=>10,
            #   :synths=>1,
            #   :groups=>7,
            #   :sdefs=>61,
            #   :avg_cpu=>0.20156468451023102,
            #   :peak_cpu=>0.36655542254447937,
            #   :nom_samp_rate=>44100.0,
            #   :act_samp_rate=>44099.9998411752,
            #   :audio_busses=>2,
            #   :control_busses=>0
            # }
"]




      def note(n, *args)
        # Short circuit out if possible.
        # Also recurse if necessary.
        case n
        when Numeric
          return n
        when Symbol
          return nil if(n == :r || n == :rest)
        when NilClass
          return nil
        when Proc
          return note(n.call, *args)
        when Hash
          return note(n[:note], *args)
        end

        return Note.resolve_midi_note_without_octave(n) if args.empty?

        args_h = resolve_synth_opts_hash_or_array(args)
        octave = args_h[:octave]
        if octave
          Note.resolve_midi_note(n, octave)
        else
          Note.resolve_midi_note_without_octave(n)
        end
      end
      doc name:          :note,
      introduced:    Version.new(2,0,0),
      summary:       "Describe note",
      doc:           "Takes a midi note, a symbol (e.g. `:C`) or a string (e.g. `\"C\"`) and resolves it to a midi note. You can also pass an optional `octave:` parameter to get the midi note for a given octave. Please note - `octave:` param overrides any octave specified in a symbol i.e. `:c3`. If the note is `nil`, `:r` or `:rest`, then `nil` is returned (`nil` represents a rest)",
      args:          [[:note, :symbol_or_number]],
      opts:          {:octave => "The octave of the note. Overrides any octave declaration in the note symbol such as :c2. Default is 4"},
      accepts_block: false,
      examples:      ["
# These all return 60 which is the midi number for middle C (octave 4)
puts note(60)
puts note(:C)
puts note(:C4)
puts note('C')
",
        "# returns 60 - octave param has no effect if we pass in a number
puts note(60, octave: 2)

# These all return 36 which is the midi number for C2 (two octaves below middle C)
puts note(:C, octave: 2)
puts note(:C4, octave: 2) # note the octave param overrides any octaves specified in a symbol
puts note('C', octave: 2)
"]




      def note_range(low_note, high_note, *opts)
        opts_h = resolve_synth_opts_hash_or_array(opts)
        low_note = note(low_note)
        high_note = note(high_note)

        potential_note_range = Range.new(low_note, high_note)

        if opts_h[:pitches]
          pitch_classes = opts_h[:pitches].map {|x| Note.resolve_note_name(x) }

          note_pool = potential_note_range.select {|n|
            pitch_classes.include? Note.resolve_note_name(n)
          }
        else
          note_pool = potential_note_range
        end

        note_pool.ring
      end
      doc name:           :note_range,
      introduced:     Version.new(2,6,0),
      summary:        "Get a range of notes",
      args:           [[:low_note, :note], [:high_note, :note]],
      returns:        :ring,
      opts:           {:pitches => "An array of notes (symbols or ints) to filter on. Octave information is ignored."},
      accepts_block:  false,
      doc:            "Produces a ring of all the notes between a low note and a high note. By default this is chromatic (all the notes) but can be filtered with a :pitches argument. This opens the door to arpeggiator style sequences and other useful patterns. If you try to specify only pitches which aren't in the range it will raise an error - you have been warned!",
      examples:       [
        "(note_range :c4, :c5) # => (ring 60,61,62,63,64,65,66,67,68,69,70,71,72)",
        "(note_range :c4, :c5, pitches: (chord :c, :major)) # => (ring 60,64,67,72)",
        "(note_range :c4, :c6, pitches: (chord :c, :major)) # => (ring 60,64,67,72,76,79,84)",
        "(note_range :c4, :c5, pitches: (scale :c, :major)) # => (ring 60,62,64,65,67,69,71,72)",
        "(note_range :c4, :c5, pitches: [:c4, :g2]) # => (ring 60,67,72)",
        "live_loop :arpeggiator do
  # try changing the chord
  play (note_range :c4, :c5, pitches: (chord :c, :major)).tick
  sleep 0.125
end"
      ]






      def note_info(n, *args)
        raise Exception.new("note_info argument must be a valid note. Got nil.") if(n.nil?)
        args_h = resolve_synth_opts_hash_or_array(args)
        octave = args_h[:octave]
        SonicPi::Note.new(n, octave)
      end
      doc name:          :note_info,
          introduced:    Version.new(2,0,0),
          summary:       "Get note info",
          doc:           "Returns an instance of `SonicPi::Note`. Please note - `octave:` param overrides any octave specified in a symbol i.e. `:c3`",
          args:          [[:note, :symbol_or_number]],
          opts:          {:octave => "The octave of the note. Overrides any octave declaration in the note symbol such as :c2. Default is 4"},
          accepts_block: false,
          examples:      [%Q{
puts note_info(:C, octave: 2)
# returns #<SonicPi::Note:0x0000010206bf78 @pitch_class="C", @octave=2, @interval=0, @midi_note=36, @midi_string="C0">}]





      def degree(degree, tonic, scale)
        Scale.resolve_degree(degree, tonic, scale)
      end
      doc name:           :degree,
          introduced:         Version.new(2,1,0),
          summary:            "Convert a degree into a note",
          doc:                "For a given scale and tonic it takes a symbol `:i`, `:ii`, `:iii`, `:iv`,`:v`, `:vi`, `:vii` or a number `1`-`7` and resolves it to a midi note.",
          args:               [[:degree, :symbol_or_number], [:tonic, :symbol], [:scale, :symbol]],
          accepts_block:      false,
          examples:           [%Q{
play degree(:ii, :D3, :major)
play degree(2, :C3, :minor)
}]




      def scale(tonic, name, *opts)
        opts = resolve_synth_opts_hash_or_array(opts)
        opts = {:num_octaves => 1}.merge(opts)
        Scale.new(tonic, name,  opts[:num_octaves]).ring
      end
      doc name:          :scale,
          introduced:    Version.new(2,0,0),
          summary:       "Create scale",
          doc:           "Creates a ring of MIDI note numbers when given a tonic note and a scale type. Also takes an optional `num_octaves:` parameter (octave `1` is the default)",
          args:          [[:tonic, :symbol], [:name, :symbol]],
          returns:        :ring,
          opts:          {:num_octaves => "The number of octaves you'd like the scale to consist of. More octaves means a larger scale. Default is 1."},
          accepts_block: false,
          intro_fn:       true,
          examples:      ["
puts scale(:C, :major) # returns the list [60, 62, 64, 65, 67, 69, 71, 72]",
        "# anywhere you can use a list of notes, you can also use scale
play_pattern scale(:C, :major)",
        "# you can use the :num_octaves parameter to get more notes
play_pattern(:C, :major, num_octaves: 2)",
        "# Sonic Pi supports a large range of scales.
use_bpm 300 # otherwise playing all these will take ages...
play_pattern scale(:C, :diatonic)
play_pattern scale(:C, :ionian)
play_pattern scale(:C, :major)
play_pattern scale(:C, :dorian)
play_pattern scale(:C, :phrygian)
play_pattern scale(:C, :lydian)
play_pattern scale(:C, :mixolydian)
play_pattern scale(:C, :aeolian)
play_pattern scale(:C, :minor)
play_pattern scale(:C, :locrian)
play_pattern scale(:C, :hex_major6)
play_pattern scale(:C, :hex_dorian)
play_pattern scale(:C, :hex_phrygian)
play_pattern scale(:C, :hex_major7)
play_pattern scale(:C, :hex_sus)
play_pattern scale(:C, :hex_aeolian)
play_pattern scale(:C, :minor_pentatonic)
play_pattern scale(:C, :yu)
play_pattern scale(:C, :major_pentatonic)
play_pattern scale(:C, :gong)
play_pattern scale(:C, :egyptian)
play_pattern scale(:C, :shang)
play_pattern scale(:C, :jiao)
play_pattern scale(:C, :zhi)
play_pattern scale(:C, :ritusen)
play_pattern scale(:C, :whole_tone)
play_pattern scale(:C, :whole)
play_pattern scale(:C, :chromatic)
play_pattern scale(:C, :harmonic_minor)
play_pattern scale(:C, :melodic_minor_asc)
play_pattern scale(:C, :hungarian_minor)
play_pattern scale(:C, :octatonic)
play_pattern scale(:C, :messiaen1)
play_pattern scale(:C, :messiaen2)
play_pattern scale(:C, :messiaen3)
play_pattern scale(:C, :messiaen4)
play_pattern scale(:C, :messiaen5)
play_pattern scale(:C, :messiaen6)
play_pattern scale(:C, :messiaen7)
play_pattern scale(:C, :super_locrian)
play_pattern scale(:C, :hirajoshi)
play_pattern scale(:C, :kumoi)
play_pattern scale(:C, :neapolitan_major)
play_pattern scale(:C, :bartok)
play_pattern scale(:C, :bhairav)
play_pattern scale(:C, :locrian_major)
play_pattern scale(:C, :ahirbhairav)
play_pattern scale(:C, :enigmatic)
play_pattern scale(:C, :neapolitan_minor)
play_pattern scale(:C, :pelog)
play_pattern scale(:C, :augmented2)
play_pattern scale(:C, :scriabin)
play_pattern scale(:C, :harmonic_major)
play_pattern scale(:C, :melodic_minor_desc)
play_pattern scale(:C, :romanian_minor)
play_pattern scale(:C, :hindu)
play_pattern scale(:C, :iwato)
play_pattern scale(:C, :melodic_minor)
play_pattern scale(:C, :diminished2)
play_pattern scale(:C, :marva)
play_pattern scale(:C, :melodic_major)
play_pattern scale(:C, :indian)
play_pattern scale(:C, :spanish)
play_pattern scale(:C, :prometheus)
play_pattern scale(:C, :diminished)
play_pattern scale(:C, :todi)
play_pattern scale(:C, :leading_whole)
play_pattern scale(:C, :augmented)
play_pattern scale(:C, :purvi)
play_pattern scale(:C, :chinese)
play_pattern scale(:C, :lydian_minor)
"]




      def chord_degree(degree, tonic, scale=:major, number_of_notes=4, *opts)
        opts = resolve_synth_opts_hash_or_array(opts)
        opts = {invert: 0}.merge(opts)

        invert_chord(Chord.resolve_degree(degree, tonic, scale, number_of_notes), opts[:invert]).ring
      end
      doc name:          :chord_degree,
          introduced:    Version.new(2,1,0),
          summary:       "Construct chords based on scale degrees",
          doc:           "A helper method that returns a ring of midi note numbers when given a degree (a symbol `:i`, `:ii`, `:iii`, `:iv`, `:v`, `:vi`, `:vii` or a number `1`-`7`), tonic, scale and number of notes",
          args:          [[:degree, :symbol_or_number], [:tonic, :symbol], [:scale, :symbol], [:number_of_notes, :number]],
          opts:          nil,
          accepts_block: false,
          examples:      ["
puts chord_degree(:i, :A3, :major) # returns a ring of midi notes - (ring 57, 61, 64, 68)
",
        "play chord_degree(:i, :A3, :major)"
      ]




      def chord(tonic, name=:major, *opts)
        return [] unless tonic
        opts = resolve_synth_opts_hash_or_array(opts)
        c = []
        if tonic.is_a? Array
          raise "List passed as parameter to chord needs two elements i.e. (chord [:e3, :minor]), you passed: #{tonic.inspect}" unless tonic.size == 2
          c = Chord.new(tonic[0], tonic[1], opts[:num_octaves])
        else
          c = Chord.new(tonic, name, opts[:num_octaves])
        end
        c = invert_chord(c, opts[:invert]) if opts[:invert]
        return c.ring
      end
      doc name:          :chord,
          introduced:    Version.new(2,0,0),
          summary:       "Create chord",
          doc:           "Creates a ring of Midi note numbers when given a tonic note and a chord type",
          args:          [[:tonic, :symbol], [:name, :symbol]],
          returns:        :ring,
          opts:          {invert: "Apply the specified num inversions to chord. See the fn `invert_chord`.",
            num_octaves: "Create an arpeggio of the chord over n octaves"},
          accepts_block: false,
          intro_fn:       true,
          examples:      ["
puts chord(:e, :minor) # returns a list of midi notes - [64, 67, 71]
",
        "# Play all the notes together
play chord(:e, :minor)",
        "# looping over arpeggios can sound good
# Here we use choose to pick a random note from the chord
loop do
  play chord(:e, :minor).choose
  sleep 0.2
end",
        "# Sonic Pi supports a large range of chords
 # Notice that the more exotic ones have to be surrounded by ' quotes
use_bpm 150 # this is just to get through all the chords more quickly
play chord(:C, '1')
sleep 1
play chord(:C, '5')
sleep 1
play chord(:C, '+5')
sleep 1
play chord(:C, 'm+5')
sleep 1
play chord(:C, :sus2)
sleep 1
play chord(:C, :sus4)
sleep 1
play chord(:C, '6')
sleep 1
play chord(:C, :m6)
sleep 1
play chord(:C, '7sus2')
sleep 1
play chord(:C, '7sus4')
sleep 1
play chord(:C, '7-5')
sleep 1
play chord(:C, 'm7-5')
sleep 1
play chord(:C, '7+5')
sleep 1
play chord(:C, 'm7+5')
sleep 1
play chord(:C, '9')
sleep 1
play chord(:C, :m9)
sleep 1
play chord(:C, 'm7+9')
sleep 1
play chord(:C, :maj9)
sleep 1
play chord(:C, '9sus4')
sleep 1
play chord(:C, '6*9')
sleep 1
play chord(:C, 'm6*9')
sleep 1
play chord(:C, '7-9')
sleep 1
play chord(:C, 'm7-9')
sleep 1
play chord(:C, '7-10')
sleep 1
play chord(:C, '9+5')
sleep 1
play chord(:C, 'm9+5')
sleep 1
play chord(:C, '7+5-9')
sleep 1
play chord(:C, 'm7+5-9')
sleep 1
play chord(:C, '11')
sleep 1
play chord(:C, :m11)
sleep 1
play chord(:C, :maj11)
sleep 1
play chord(:C, '11+')
sleep 1
play chord(:C, 'm11+')
sleep 1
play chord(:C, '13')
sleep 1
play chord(:C, :m13)
sleep 1
play chord(:C, :major)
sleep 1
play chord(:C, :M)
sleep 1
play chord(:C, :minor)
sleep 1
play chord(:C, :m)
sleep 1
play chord(:C, :major7)
sleep 1
play chord(:C, :dom7)
sleep 1
play chord(:C, '7')
sleep 1
play chord(:C, :M7)
sleep 1
play chord(:C, :minor7)
sleep 1
play chord(:C, :m7)
sleep 1
play chord(:C, :augmented)
sleep 1
play chord(:C, :a)
sleep 1
play chord(:C, :diminished)
sleep 1
play chord(:C, :dim)
sleep 1
play chord(:C, :i)
sleep 1
play chord(:C, :diminished7)
sleep 1
play chord(:C, :dim7)
sleep 1
play chord(:C, :i7)
sleep 1
"]




      def invert_chord(notes, shift)
        raise "Inversion shift value must be a number, got #{shift.inspect}" unless shift.is_a?(Numeric)
        raise "Notes must be a list of notes, got #{notes.inspect}" unless (notes.is_a?(SonicPi::Core::RingVector) || notes.is_a?(Array))
        if(shift > 0)
          invert_chord(notes.to_a[1..-1] + [notes.to_a[0]+12], shift-1)
        elsif(shift < 0)
          invert_chord((notes.to_a[0..-2] + [notes.to_a[-1]-12]).sort, shift+1)
        else
          notes.ring
        end
      end
      doc name:          :invert_chord,
          introduced:    Version.new(2,6,0),
          summary:       "Invert a chord",
          doc:           "Given a set of notes, apply a number of inversions indicated by Shift. Inversions being an increase to notes if Shift is positive or decreasing the notes if Shift is negative.",
          args:          [[:notes, :list], [:shift, :number]],
          returns:        :ring,
          opts:          nil,
          accepts_block: false,
          examples:      ["
play invert_chord(chord(:A3, \"M\"), 0) #No inversion
sleep 1
play invert_chord(chord(:A3, \"M\"), 1) #First chord inversion
sleep 1
play invert_chord(chord(:A3, \"M\"), 2) #Second chord inversion
"]




      def control(node, *args)
        ensure_good_timing!
        return nil if node.nil?

        args_h = resolve_synth_opts_hash_or_array(args)
        n = args_h[:note]
        args_h[:note] = note(n) if n
        notes = args_h[:notes]
        if node.is_a?(ChordGroup) && notes
          # don't normalise notes key as it is special
          # when controlling ChordGroups.
          # TODO: remove this hard coded behaviour
          args_h.delete(:notes)
          normalise_args! args_h
          args_h[:notes] = notes.map{|n| note(n)}
        else
          normalise_args! args_h
        end

        # set default slide times
        default_slide_time = args_h[:slide]
        args_h.delete :slide
        if node.info && default_slide_time
          node.info.slide_args.each do |k|
            args_h[k] = default_slide_time unless args_h.has_key?(k)
          end
        end

        node.control args_h
        unless Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_silent)
          __delayed_message "control node #{node.id}, #{arg_h_pp(args_h)}"
        end

      end
      doc name:          :control,
          introduced:    Version.new(2,0,0),
          summary:       "Control running synth",
          doc:           "Control a running synth node by passing new parameters to it. A synth node represents a running synth and can be obtained by assigning the return value of a call to play or sample or by specifying a parameter to the do/end block of an FX. You may modify any of the parameters you can set when triggering the synth, sample or FX. See documentation for opt details. If the synth to control is a chord, then control will change all the notes of that chord group at once to a new target set of notes - see example. ",
          args:          [[:node, :synth_node]],
          opts:          {},
          accepts_block: false,
          examples:      ["
## Basic control

my_node = play 50, release: 5, cutoff: 60 # play note 50 with release of 5 and cutoff of 60. Assign return value to variable my_node
sleep 1 # Sleep for a second
control my_node, cutoff: 70 # Now modify cutoff from 60 to 70, sound is still playing
sleep 1 # Sleep for another second
control my_node, cutoff: 90 # Now modify cutoff from 70 to 90, sound is still playing",
        "
## Combining control with slide opts allows you to create nice transitions.

s = synth :prophet, note: :e1, cutoff: 70, cutoff_slide: 8, release: 8 # start synth and specify slide time for cutoff opt
control s, cutoff: 130 # Change the cutoff value with a control.
                       # Cutoff will now slide over 8 beats from 70 to 130",

        "
## Use a short slide time and many controls to create a sliding melody

notes = (scale :e3, :minor_pentatonic, num_octaves: 2).shuffle # get a random ordering of a scale

s = synth :beep, note: :e3, sustain: 8, note_slide: 0.05 # Start our synth running with a long sustain and short note slide time
64.times do
  control s, note: notes.tick                            # Keep quickly changing the note by ticking through notes repeatedly
  sleep 0.125
end
",

        "
## Controlling FX

with_fx :bitcrusher, sample_rate: 1000, sample_rate_slide: 8 do |bc| # Start FX but also use the handy || goalposts
                                                                     # to grab a handle on the running FX. We can call
                                                                     # our handle anything we want. Here we've called it bc
  sample :loop_garzul, rate: 1
  control bc, sample_rate: 5000                                      # We can use our handle bc now just like we used s in the
                                                                     # previous example to modify the FX as it runs.
end",
        "
## Controlling chords
cg = play (chord :e4, :minor), sustain: 2  # start a chord
sleep 1
control cg, notes: (chord :c3, :major)     # transition to new chord.
                                           # Each note in the original chord is mapped onto
                                           # the equivalent in the new chord.
",
        "
## Sliding between chords

cg = play (chord :e4, :minor), sustain: 4, note_slide: 3  # start a chord
sleep 1
control cg, notes: (chord :c3, :major)                    # slide to new chord.
                                                          # Each note in the original chord is mapped onto
                                                          # the equivalent in the new chord.
",
        "
## Sliding from a larger to smaller chord
cg = play (chord :e3, :m13), sustain: 4, note_slide: 3  # start a chord with 7 notes
sleep 1
control cg, notes: (chord :c3, :major)                    # slide to new chord with fewer notes (3)
                                                          # Each note in the original chord is mapped onto
                                                          # the equivalent in the new chord using ring-like indexing.
                                                          # This means that the 4th note in the original chord will
                                                          # be mapped onto the 1st note in the second chord and so-on.
",
        "
## Sliding from a smaller to larger chord
cg = play (chord :c3, :major), sustain: 4, note_slide: 3  # start a chord with 3 notes
sleep 1
control cg, notes: (chord :e3, :m13)                     # slide to new chord with more notes (7)
                                                          # Each note in the original chord is mapped onto
                                                          # the equivalent in the new chord.
                                                          # This means that the 4th note in the new chord
                                                          # will not sound as there is no 4th note in the
                                                          # original chord.
"
      ]




      def kill(node)
        ensure_good_timing!
        return nil if node.nil?

        alive = node.live?
        node.kill
        if alive
          __delayed_message "killing sound #{node.id}"
        else
          __delayed_message "not killing sound #{node.id} (already killed)"
        end
      end
      doc name:          :kill,
          introduced:    Version.new(2,0,0),
          summary:       "Kill synth",
          doc:           "Kill a running synth sound or sample. In order to kill a sound, you need to have stored a reference to it in a variable.",
          args:          [[:node, :synth_node]],
          opts:          {},
          accepts_block: false,
          examples:      ["
# store a reference to a running synth in a variable called foo:
foo = play 50, release: 4
sleep 1
# foo is still playing, but we can kill it early:
kill foo
",
        "bar = sample :loop_amen
sleep 0.5
kill bar"]




      def sample_names(group)
        BaseInfo.grouped_samples[group][:samples]
      end
      doc name:          :sample_names,
          introduced:    Version.new(2,0,0),
          summary:       "Get sample names",
          doc:           "Return a list of sample names for the specified group",
          args:          [[:group, :symbol]],
          opts:          nil,
          accepts_block: false,
          examples:      []




      def all_sample_names
        BaseInfo.all_samples
      end
      doc name:          :all_sample_names,
          introduced:    Version.new(2,0,0),
          summary:       "Get all sample names",
          doc:           "Return a list of all the sample names available",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      []




      def sample_groups
        BaseInfo.grouped_samples.keys
      end
      doc name:          :sample_groups,
          introduced:    Version.new(2,0,0),
          summary:       "Get all sample groups",
          doc:           "Return a list of all the sample groups available",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      []




      def load_synthdefs(path=synthdef_path)
        path = File.expand_path(path)
        raise "No directory exists called #{path.inspect}" unless File.exists? path
        @mod_sound_studio.load_synthdefs(path)
        __info "Loaded synthdefs in path #{path}"
      end
      doc name:          :load_synthdefs,
          introduced:    Version.new(2,0,0),
          summary:       "Load external synthdefs",
          doc:           "Load all pre-compiled synth designs in the specified directory. The binary files containing synth designs need to have the extension `.scsyndef`. This is useful if you wish to use your own SuperCollider synthesiser designs within Sonic Pi.

## Important note

If you wish your synth to work with Sonic Pi's automatic stereo sound infrastructure *you need to ensure your synth outputs a stereo signal* to an audio bus with an index specified by a synth arg named `out_bus`. For example, the following synth would work nicely:


    (
    SynthDef(\piTest,
             {|freq = 200, amp = 1, out_bus = 0 |
               Out.ar(out_bus,
                      SinOsc.ar([freq,freq],0,0.5)* Line.kr(1, 0, 5, amp, doneAction: 2))}
    ).store;
    )
    ",
      args:          [[:path, :string]],
      opts:          nil,
      accepts_block: false,
      examples:      ["load_synthdefs \"~/Desktop/my_noises\" # Load all synthdefs in my_noises folder"]


      def scale_names
        Scale::SCALE.keys.ring
      end
      doc name:          :scale_names,
          introduced:    Version.new(2,6,0),
          summary:       "All scale names",
          doc:           "Returns a ring containing all scale names known to Sonic Pi",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      ["puts scale_names #=>  prints a list of all the scales"]


      def chord_names
        Chord::CHORD.keys.ring
      end
      doc name:          :chord_names,
          introduced:    Version.new(2,6,0),
          summary:       "All chord names",
          doc:           "Returns a ring containing all chord names known to Sonic Pi",
          args:          [],
          opts:          nil,
          accepts_block: false,
          examples:      ["puts chord_names #=>  prints a list of all the chords"]

      private

      def normalise_args!(args_h)
        args_h.keys.each do |k|
          v = args_h[k]
          case v
          when Fixnum, Float
            # do nothing
          when Proc
            args_h[k] = v.call.to_f
          when Symbol
            # Allow vals to be keys to other vals
            # But only one level deep...
            args_h[k] = args_h[v].to_f
          when TrueClass
            args_h[k] = 1.0
          when FalseClass
            args_h[k] = 0.0
          when NilClass
            args_h[k] = 0.0
          else
            begin
              args_h[k] = v.to_f
            rescue
              raise "Unable to normalise argument with key #{k.inspect} and value #{v.inspect}"
            end
          end
        end
        args_h
      end

      def scale_time_args_to_bpm!(args_h, info)
        # some of the args in args_h need to be scaled to match the
        # current bpm. Check in info to see if that's necessary and if
        # so, scale them.
        info.bpm_scale_args.each do |arg_name|
          if args_h.has_key? arg_name
            args_h[arg_name] = args_h[arg_name] * Thread.current.thread_variable_get(:sonic_pi_spider_sleep_mul)
          end

        end
        args_h
      end

      def find_sample_with_path(path)
        ["wav", "aiff", "aif", "wave"].each do |ext|
          full = "#{path}.#{ext}"
          return full if File.exists?(full)
        end
        return nil
      end

      def fetch_or_cache_sample_path(sym)
        cached = @sample_paths_cache[sym]
        return cached if cached

        res = find_sample_with_path("#{samples_path}/#{sym.to_s}")

        raise "No sample exists called :#{sym} in default sample pack" unless res
        @sample_paths_cache[sym] = res
        res
      end

      def resolve_sample_symbol_path(sym)
        aliases = Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_aliases)
        path = Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_path)

        return fetch_or_cache_sample_path(sym) unless (aliases || path)

        if (aliases &&
            (m       = sym.to_s.match /\A(.+?)__(.+)/) &&
            (p       = aliases[m[1]]))
          path = p
          sym = m[2]
          partial = "#{p}#{sym}"
        elsif path
          partial = path + sym.to_s
        else
          path = samples_path
          partial = path + "/" + sym.to_s
        end

        res = find_sample_with_path(partial)

        raise "No sample exists called #{sym.inspect} in sample pack #{path.inspect} (#{File.expand_path(path)})" unless res

        res
      end

      def complex_sampler_args?(args_h)
        # break out early if any of the 'complex' keys exist in the
        # args map:
        return false if args_h.empty?
        return !(args_h.keys - @simple_sampler_args).empty?
      end


      def trigger_sampler(path, buf_id, num_chans, args_h, group=current_job_synth_group)
        if complex_sampler_args?(args_h)
          #complex
          synth_name = (num_chans == 1) ? :mono_player : :stereo_player
        else
          #basic
          synth_name = (num_chans == 1) ? :basic_mono_player : :basic_stereo_player
        end

        trigger_specific_sampler(synth_name, path, buf_id, num_chans, args_h, group)
      end

      def trigger_specific_sampler(sampler_type, path, buf_id, num_chans, args_h, group=current_job_synth_group)
        args_h_with_buf = {:buf => buf_id}.merge(args_h)
        sn = sampler_type.to_sym
        info = SynthInfo.get_info(sn)
        validate_if_necessary! info, args_h
        path = path.gsub(/\A#{@mod_sound_home_dir}/, "~") if path.is_a? String
        unless Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_silent)
          if args_h.empty?
            __delayed_message "sample #{path.inspect}"
          else
            __delayed_message "sample #{path.inspect}, #{arg_h_pp(args_h)}"
          end
        end

        # Combine thread local defaults here as
        # normalise_and_resolve_synth_args has only been taught about
        # synth thread local defaults
        t_l_args = Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_defaults) || {}
        args_h_with_buf = t_l_args.merge(args_h_with_buf)
        trigger_synth(sn, args_h_with_buf, group, info)
      end

      def trigger_inst(synth_name, args_h, group=current_job_synth_group)
        sn = synth_name.to_sym
        info = SynthInfo.get_info(sn)

        unless Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_silent)
          __delayed_message "synth #{sn.inspect}, #{arg_h_pp(args_h)}"
        end

        trigger_synth(synth_name, args_h, group, info, false, nil, true)
      end

      def trigger_chord(synth_name, notes, args_a_or_h, group=current_job_synth_group)
        sn = synth_name.to_sym
        info = SynthInfo.get_info(sn)
        args_h = resolve_synth_opts_hash_or_array(args_a_or_h)
        args_h = normalise_and_resolve_synth_args(args_h, info, nil, true)

        chord_group = @mod_sound_studio.new_group(:tail, group, "CHORD")
        cg = ChordGroup.new(chord_group, notes)

        unless Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_silent)
          __delayed_message "synth #{sn.inspect}, #{arg_h_pp(args_h.merge({note: notes}))}"
        end

        # Scale down amplitude based on number of notes in chord
        amp = args_h[:amp] || 1.0
        args_h[:amp] = amp.to_f / notes.size

        nodes = []
        notes.each do |note|
          if note
            args_h[:note] = note
            nodes << trigger_synth_with_resolved_args(synth_name, args_h, cg, info)
          end
        end
        cg.sub_nodes = nodes
        cg
      end

      def trigger_fx(synth_name, args_h, info, in_bus, group=current_fx_group, now=false, t_minus_delta=false)
        n = trigger_synth_with_resolved_args(synth_name, args_h, group, info, now, t_minus_delta)
        FXNode.new(n, in_bus, current_out_bus)
      end

      def trigger_synth(synth_name, args_h, group, info, now=false, out_bus=nil, combine_tls=false)

        # set default slide times
        default_slide_time = args_h[:slide]
        if info && default_slide_time
          info.slide_args.each do |k|
            args_h[k] = default_slide_time unless args_h.has_key?(k)
          end
        end

        processed_args = normalise_and_resolve_synth_args(args_h, info, out_bus, combine_tls)
        trigger_synth_with_resolved_args(synth_name, processed_args, group, info, now, out_bus)
      end

      # Function that actually triggers synths now that all args are resolved
      def trigger_synth_with_resolved_args(synth_name, args_h, group, info, now=false, out_bus=nil, t_minus_delta=false)
        synth_name = info ? info.scsynth_name : synth_name
        validate_if_necessary! info, args_h
        job_id = current_job_id
        __no_kill_block do

          p = Promise.new
          job_synth_proms_add(job_id, p)

          s = @mod_sound_studio.trigger_synth synth_name, group, args_h, info, now, t_minus_delta

          trackers = Thread.current.thread_variable_get(:sonic_pi_mod_sound_trackers)

          if trackers
            s.on_started do
              trackers.each{|t| t.synth_started(s)}
            end
          end

          s.on_destroyed do
            trackers.each{|t| t.synth_finished(s)} if trackers
            job_synth_proms_rm(job_id, p)
            p.deliver! true
          end

          s
        end
      end

      def normalise_and_resolve_synth_args(args_h, info, out_bus=nil, combine_tls=false)
        defaults = info ? info.arg_defaults : {}
        unless out_bus
          out_bus = current_out_bus
        end

        if combine_tls
          t_l_args = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults) || {}
          combined_args = defaults.merge(t_l_args.merge(args_h))
        else
          combined_args = defaults.merge(args_h)
        end

        combined_args["out_bus"] = out_bus
        combined_args[:rand_buf] = @mod_sound_studio.rand_buf_id if combined_args[:seed]
        normalise_args!(combined_args)
        scale_time_args_to_bpm!(combined_args, info) if info && Thread.current.thread_variable_get(:sonic_pi_spider_arg_bpm_scaling)
        combined_args
      end

      def current_job_id
        Thread.current.thread_variable_get :sonic_pi_spider_job_id
      end

      def current_job_mixer
        job_mixer(current_job_id)
      end

      def current_fx_main_group
        if g = Thread.current.thread_variable_get(:sonic_pi_mod_sound_fx_main_group)
          return g
        else
          g = job_fx_group(current_job_id)
          Thread.current.thread_variable_set :sonic_pi_mod_sound_fx_main_group, g
          return g
        end
      end

      def current_fx_group
        Thread.current.thread_variable_get(:sonic_pi_mod_sound_fx_group) || current_fx_main_group
      end

      def current_job_synth_group
        if g = Thread.current.thread_variable_get(:sonic_pi_mod_sound_job_group)
          return g
        else
          g = job_synth_group(current_job_id)
          Thread.current.thread_variable_set :sonic_pi_mod_sound_job_group, g
          return g
        end
      end

      def current_out_bus
        current_bus = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_out_bus)
        current_bus || current_job_bus
      end

      def current_job_bus
        job_bus(current_job_id)
      end

      def job_bus(job_id)
        b = @JOB_BUSSES_A.deref[job_id]
        return b if b

        new_bus = nil

        @JOB_BUSSES_MUTEX.synchronize do
          b = @JOB_BUSSES_A.deref[job_id]
          return b if b

          begin
            new_bus = @mod_sound_studio.new_fx_bus
          rescue AllocationError
            raise "All busses allocated - unable to create audio bus for job"
          end

          @JOB_BUSSES_A.swap! do |gs|
            gs.put job_id, new_bus
          end
        end
        ## ensure job mixer has started
        job_mixer(job_id)
        return new_bus
      end

      def job_mixer(job_id)
        m = @JOB_MIXERS_A.deref[job_id]
        return m if m

        @JOB_MIXERS_MUTEX.synchronize do
          m = @JOB_MIXERS_A.deref[job_id]
          return m if m

          args_h = {
            "in_bus" => job_bus(job_id).to_i,
            "amp" => 0.3
          }

          sn = "basic_mixer"
          info = SynthInfo.get_info(sn)
          defaults = info.arg_defaults
          synth_name = info.scsynth_name

          combined_args = defaults.merge(args_h)
          combined_args["out_bus"] = @mod_sound_studio.mixer_bus.to_i

          validate_if_necessary! info, combined_args

          group = @mod_sound_studio.mixer_group

          n = @mod_sound_studio.trigger_synth synth_name, group, combined_args, info, true

          mix_n = ChainNode.new(n)

          @JOB_MIXERS_A.swap! do |gs|
            gs.put job_id, mix_n
          end

          return mix_n
        end
      end


      def job_synth_group(job_id)
        g = @JOB_GROUPS_A.deref[job_id]
        return g if g

        @JOB_GROUP_MUTEX.synchronize do
          g = @JOB_GROUPS_A.deref[job_id]
          return g if g
          g = @mod_sound_studio.new_synth_group(job_id)

          @JOB_GROUPS_A.swap! do |gs|
            gs.put job_id, g
          end
        end
        g
      end

      def job_fx_group(job_id)
        g = @JOB_FX_GROUPS_A.deref[job_id]


        return g if g

        @JOB_FX_GROUP_MUTEX.synchronize do
          g = @JOB_FX_GROUPS_A.deref[job_id]
          return g if g
          g = @mod_sound_studio.new_fx_group(job_id)

          @JOB_FX_GROUPS_A.swap! do |gs|
            gs.put job_id, g
          end
        end
        g
      end

      def job_synth_proms_add(job_id, p)
        q = @job_proms_queues[job_id]
        q << [:started, p]
      end

      def job_synth_proms_rm(job_id, p)
        q = @job_proms_queues[job_id]
        q << [:completed, p]
      end

      def free_job_bus(job_id)
        old_job_busses = @JOB_BUSSES_A.swap_returning_old! do |js|
          js.delete job_id
        end
        bus = old_job_busses[job_id]
        bus.free if bus
      end

      def shutdown_job_mixer(job_id)
        old_job_mixers = @JOB_MIXERS_A.swap_returning_old! do |js|
          js.delete job_id
        end
        mixer = old_job_mixers[job_id]
        if mixer
          mixer.ctl_now amp_slide: 1
          Kernel.sleep 0.1
          mixer.ctl_now amp: 0
          Kernel.sleep 1
          mixer.kill(true)
        end

      end

      def kill_job_group(job_id)

        old_job_groups = @JOB_GROUPS_A.swap_returning_old! do |js|
          js.delete job_id
        end
        job_group = old_job_groups[job_id]
        job_group.kill(true) if job_group

      end

      def kill_fx_job_group(job_id)
        old_job_groups = @JOB_FX_GROUPS_A.swap_returning_old! do |js|
          js.delete job_id
        end
        job_group = old_job_groups[job_id]
        job_group.kill(true) if job_group
      end

      def join_thread_and_subthreads(t)
        t.join
        subthreads = t.thread_variable_get :sonic_pi_spider_subthreads
        subthreads.each do |st|
          join_thread_and_subthreads(st)
        end
      end

      def job_proms_joiner(job_id)
        all_proms_joined = Promise.new
        prom_queue = @job_proms_queues[job_id]

        raise "whoops, no prom_queue!" unless prom_queue

        Thread.new do
          Thread.current.thread_variable_set(:sonic_pi_thread_group, "job_#{job_id}_prom_joiner")
          Thread.current.priority = -10

          proms = []

          # Pull messages from queue and either add to proms array or
          # remove depending on whether the synth started or completed.
          while (p = prom_queue.pop) != :job_finished
            action, prom = *p
            if action == :started
              proms << prom
            else
              proms.delete prom
            end
          end

          proms.each do |p|
            p.get
          end
          @job_proms_queues_mut.synchronize do
            @job_proms_queues.delete job_id
          end
          all_proms_joined.deliver!(true)
        end

        return all_proms_joined
      end

      def validate_if_necessary!(info, args_h)
        if info &&
            Thread.current.thread_variable_get(:sonic_pi_mod_sound_check_synth_args)
          info.validate!(args_h)
        end
      end

      def ensure_good_timing!
        return true if Thread.current.thread_variable_get(:sonic_pi_mod_sound_disable_timing_warnings)

        vt  = Thread.current.thread_variable_get :sonic_pi_spider_time
        sat = @mod_sound_studio.sched_ahead_time + 1.1
        raise "Timing Exception: thread got too far behind time." if (Time.now - sat) > vt
      end

      def current_synth_name
        Thread.current.thread_variable_get(:sonic_pi_mod_sound_current_synth_name) ||
        Thread.current.thread_variable_set(:sonic_pi_mod_sound_current_synth_name, "beep")
      end

      def set_current_synth(name)
        Thread.current.thread_variable_set(:sonic_pi_mod_sound_current_synth_name, name)
      end

      def __freesound_path(id)
        cache_dir = home_dir + '/freesound/'
        ensure_dir(cache_dir)

        cache_file = cache_dir + "freesound-" + id.to_s + ".wav"

        return cache_file if File.exists?(cache_file)

        __info "Caching freesound #{id}..."

        in_thread(name: "download_freesound_#{id}".to_sym) do
          # API key borrowed from Overtone
          apiURL = 'http://www.freesound.org/api/sounds/' + id.to_s + '/serve/?api_key=47efd585321048819a2328721507ee23'

          resp = Net::HTTP.get_response(URI(apiURL))
          case resp
          when Net::HTTPSuccess then
            if not resp['Content-Disposition'] =~ /\.wav\"$/ then
              raise 'Only WAV freesounds are supported, sorry!'
            end

            open(cache_file, 'wb') do |file|
              file.write(resp.body)
            end
            __info "Freesound #{id} loaded and ready to fire!"
          else
            __info "Failed to download freesound #{id}: " + resp.value
          end
        end
        return nil
      end
      #        doc name:          :freesound_path,
      #            introduced:    Version.new(2,1,0),
      #            summary:       "Return local path for sound from freesound.org",
      #            doc:           "Download and cache a sample by ID from freesound.org. Returns path as string if cached. If not cached, returns nil and starts a background thread to download the sample.",
      #            args:          [[:id, :number]],
      #            opts:          nil,
      #            accepts_block: false,
      #            examples:      ["
      # puts freesound(250129)    # preloads a freesound and prints its local path, such as '/home/user/.sonic_pi/freesound/250129.wav'"]

      def __freesound(id, *opts)
        path = __freesound_path(id)
        arg_h = resolve_synth_opts_hash_or_array(opts)
        fallback = arg_h[:fallback]

        if path
          sample path
        elsif fallback
          raise "Freesound fallback must be a symbol" unless fallback.is_a? Symbol
          __info "Freesound #{id} not yet loaded, playing #{fallback}"
          sample fallback
        else
          __info "Freesound #{id} not yet loaded, skipping"
        end

      end
      #        doc name:          :freesound,
      #            introduced:    Version.new(2,1,0),
      #            summary:       "Play sample from freesound.org",
      #            doc:           "Fetch from cache (or download then cache) a sample by ID from freesound.org, and then play it.",
      #            args:          [[:id, :number]],
      #            opts:          {:fallback => "Symbol representing built-in sample to play if the freesound id isn't yet downloaded"},
      #            accepts_block: false,
      #            examples:      ["
      # freesound(250129)  # takes time to download the first time, but then the sample is cached locally
      # ",
      # "
      # loop do
      #   sample freesound(27130)
      #   sleep sample_duration(27130)
      # end
      # "
      # ]
    end
  end
end
