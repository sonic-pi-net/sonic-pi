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
require 'tmpdir'
require 'fileutils'
require 'thread'
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

module SonicPi
   module Mods
     module Sound

       include SonicPi::Util
       include SonicPi::DocSystem

       DEFAULT_PLAY_OPTS = {amp:      {default: 1, doc: "The amplitude of the note"},
                           amp_slide: {default: 0, doc: "The duration in seconds for amplitude changes to take place"},
                           pan:       {default: 0, doc: "The stereo position of the sound. -1 is left, 0 is in the middle and 1 is on the right. You may use value in between -1 and 1 such as 0.25"},
                           pan_slide: {default: 0, doc: "The duration in seconds for the pan value to change"},
                           attack:    {default: :synth_specific, doc: "The duration in seconds for the sound to reach maximum amplitude. Choose short values for percusive sounds and long values for a fade-in effect."},
                           sustain:   {default: 0, doc: "The duration in seconds for the sound to stay at full amplitude. Used to give the sound duration"},
                           release:   {default: :synth_specific, doc: "The duration in seconds for the sound to fade out."}}

       def self.included(base)
         base.instance_exec {alias_method :sonic_pi_mods_sound_initialize_old, :initialize}

         base.instance_exec do
           define_method(:initialize) do |*splat, &block|
             sonic_pi_mods_sound_initialize_old *splat, &block
             hostname, port, msg_queue, max_concurrent_synths = *splat
             @complex_sampler_args = [:attack, :decay, :sustain, :release, :start, :finish, :env_curve, :attack_level, :sustain_level]

             @blank_node = BlankNode.new
             @job_proms_queues = {}
             @job_proms_queues_mut = Mutex.new

             @job_proms_joiners = {}

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
             end
           end
         end
       end




    def use_arg_bpm_scaling(bool, &block)
      raise "use_arg_bpm_scaling does not work with a block. Perhaps you meant with_arg_bpm_scaling" if block
      Thread.current.thread_variable_set(:sonic_pi_spider_arg_bpm_scaling, bool)
    end
    doc name:           :use_arg_bpm_scaling,
        introduced:     Version.new(2,0,0),
        summary:        "Enable and disable BPM scaling",
        doc:            "Turn synth argument bpm scaling on or off for the current thread. This is on by default. Note, using rt for args will result in incorrect times when used after turning arg bpm scaling off.",
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
      raise "with_arg_bpm_scaling must be called with a block. Perhaps you meant use_arg_bpm_scaling" unless block
      current_scaling = Thread.current.thread_variable_get(:sonic_pi_spider_arg_bpm_scaling)

      Thread.current.thread_variable_set(:sonic_pi_spider_arg_bpm_scaling, bool)
      block.call
      Thread.current.thread_variable_set(:sonic_pi_spider_arg_bpm_scaling, current_scaling)
    end
    doc name:           :with_arg_bpm_scaling,
        introduced:     Version.new(2,0,0),
        summary:        "Block-level enable and disable BPM scaling",
        doc:            "Turn synth argument bpm scaling on or off for the supplied block. Note, using rt for args will result in incorrect times when used within this block.",
        args:           [],
        opts:           nil,
        accepts_block:  true,
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
           accepts_block: false,
           examples:      ["set_sched_ahead_time! 1 # Code will now run approximately 1 second ahead of audio."]




       def use_debug(v, &block)
         raise "use_debug does not work with a do/end block. Perhaps you meant with_debug" if block
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_synth_silent, !v)
       end
       doc name:          :use_debug,
           introduced:    Version.new(2,0,0),
           summary:       "Enable and disable debug",
           doc:           "Enable or disable messages created on synth triggers. If this is set to false, the synths will be silent until debug is turned back on. Silencing debug messages can reduce output noise and also increase performance on slower platforms. See with_debug for setting the debug value only for a specific do/end block.",
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
           doc:           "Similar to use_debug except only applies to code within supplied do/end block. Previous debug value is restored after block.",
           args:          [[:true_or_false, :boolean]],
           opts:          nil,
           accepts_block: true,
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
         raise "use_arg_checks does not work with a a do/end block. Perhaps you meant use_arg_checks" if block

         Thread.current.thread_variable_set(:sonic_pi_mod_sound_check_synth_args, !!v)
       end
       doc name:          :use_arg_checks,
           introduced:    Version.new(2,0,0),
           summary:       "Enable and disable arg checks",
           doc:           "When triggering synths, each argument is checked to see if it is sensible. When argument checking is enabled and an argument isn't sensible, you'll see an error in the debug pane. This setting allows you to explicitly enable and disable the checking mechanism. See with_arg_checks for enabling/sisabling argument checking only for a specific do/end block.",
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
       doc name:          :with_arg_checks,
           introduced:    Version.new(2,0,0),
           summary:       "Block-level enable and disable arg checks",
           doc:           "Similar to use_arg_checks except only applies to code within supplied do/end block. Previous arg check value is restored after block.",
           args:          [[:true_or_false, :boolean]],
           opts:          nil,
           accepts_block: true,
           examples:      ["
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
           doc:           "Transposes your music by shifting all notes played by the specified amount. To shift up by a semitone use a transpose of 1. To shift down use negative numbers. See with_transpose for setting the transpose value only for a specific do/end block.",
           args:          [[:note_shift, :number]],
           opts:          nil,
           accepts_block: false,
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
       doc name:          :with_transpose,
           introduced:    Version.new(2,0,0),
           summary:       "Block-level note transposition",
           doc:           "Similar to use_transpose except only applies to code within supplied do/end block. Previous transpose value is restored after block.",
           args:          [[:note_shift, :number]],
           opts:          nil,
           accepts_block: true,
           examples:      ["
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




       def use_synth(synth_name, &block)
         raise "use_synth does not work with a do/end block. Perhaps you meant with_synth" if block
         set_current_synth synth_name
       end
       doc name:          :use_synth,
           introduced:    Version.new(2,0,0),
           summary:       "Switch current synth",
           doc:           "Switch the current synth to synth_name. Affects all further calls to play. See with_synth for changing the current synth only for a specific do/end block.",
           args:          [[:synth_name, :symbol]],
           opts:          nil,
           accepts_block: false,
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
       doc name:          :with_synth,
           introduced:    Version.new(2,0,0),
           summary:       "Block-level synth switching",
           doc:           "Switch the current synth to synth_name but only for the duration of the do/end block. After the do/end block has completed, the previous synth is restored.",
           args:          [[:synth_name, :symbol]],
           opts:          nil,
           accepts_block: true,
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
         __info "Start recording"
         tmp_dir = Dir.mktmpdir("sonic-pi")
         @tmp_path = File.expand_path("#{tmp_dir}/#{Random.rand(100000000)}.wav")
         @mod_sound_studio.recording_start @tmp_path
       end
       doc name:          :recording_start,
           introduced:    Version.new(2,0,0),
           summary:       "Start recording",
           doc:           "Start recording all sound to a wav file stored in a temporary directory.",
           args:          [],
           opts:          nil,
           accepts_block: false,
           examples:      [],
           hide:          true




       def recording_stop
         __info "Stop recording"
         @mod_sound_studio.recording_stop
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
         __info "Saving recording to #{filename}"
         FileUtils.mv(@tmp_path, filename)
         @tmp_path = nil
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
           doc:           "After using recording_start and recording_stop, a temporary file is created until you decide to use recording_save. If you've decided you don't want to save it you can use this method to delete the temporary file straight away, otherwise the operating system will take care of it later.",
           args:          [],
           opts:          nil,
           accepts_block: false,
           examples:      [],
           hide:          true




       def synth(synth_name, *args)
         ensure_good_timing!
         args_h = resolve_synth_opts_hash_or_array(args)

         if args_h.has_key? :note
           n = args_h[:note]
           n = n.call if n.is_a? Proc
           n = note(n) unless n.is_a? Numeric
           if shift = Thread.current.thread_variable_get(:sonic_pi_mod_sound_transpose)
             n += shift
           end
           args_h[:note] = n
         end

         trigger_inst synth_name, args_h
       end
       doc name: :synth,
           introduced:    Version.new(2,0,0),
           summary:       "Trigger specific synth",
           doc: "Trigger specified synth with given arguments. Bypasses current synth value, yet still honours synth defaults. ",
           args:  [[:synth_name, :symbol]],
           opts:  {},
           accepts_block: false,
           examples: ["
synth :fm, note: 60, amp: 0.5 # Play note 60 of the :fm synth with an aplitude of 0.5",

"
use_synth_defaults release: 5
synth :dsaw, note: 50 # Play note 50 of the :dsaw synth with a release of 5"]




       def play(n, *args)
         ensure_good_timing!
         return play_chord(n, *args) if n.is_a?(Array)
         return nil if (n.nil? || n == :r || n == :rest)

         n = n.call if n.is_a? Proc
         n = note(n) unless n.is_a? Numeric
         args_h = resolve_synth_opts_hash_or_array(args)
         if shift = Thread.current.thread_variable_get(:sonic_pi_mod_sound_transpose)
           n += shift
         end
         args_h[:note] = n
         trigger_inst current_synth_name, args_h
       end
       doc name:          :play,
           introduced:    Version.new(2,0,0),
           summary:       "Play current synth",
           doc:           "Play note with current synth. Accepts a set of standard options which include control of an amplitude envelope with attack, sustain and release phases. These phases are triggered in order, so the duration of the sound is attack + sustain + release times. The duration of the sound does not affect any other notes. Code continues executing whilst the sound is playing through its envelope phases.

Accepts optional args for modification of the synth being played. See each synth's documentation for synth-specific opts. See use_synth and with_synth for changing the current synth.

If note is nil, :r or :rest, play is ignored and treated as a rest.
",
           args:          [[:note, :symbol_or_number]],
           opts:          DEFAULT_PLAY_OPTS,
           accepts_block: false,
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
           doc:           "Play each note in a list of notes one after another with specified times between them. The notes should be a list of MIDI numbers or symbols such as :E4 - identical to the first parameter of the play function. The times should be a list of times between the notes in seconds.

If the list of times is smaller than the number of gaps between notes, the list is repeated again. If the list of times is longer than the number of gaps between notes, then some of the times are ignored. See examples for more detail.

Accepts optional args for modification of the synth being played. See each synth's documentation for synth-specific opts. See use_synth and with_synth for changing the current synth.",
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
play 55
sleep 0.5",

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

Accepts optional args for modification of the synth being played. See each synth's documentation for synth-specific opts. See use_synth and with_synth for changing the current synth.",
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
play 50 #=> Plays note 50 with amp 0.8, cutoff 80 and pan -1"]





       def with_merged_synth_defaults(*args, &block)
         raise "with_merged_synth_defaults must be called with a block" unless block
         current_defs = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults)

         args_h = resolve_synth_opts_hash_or_array(args)
         merged_defs = (current_defs || {}).merge(args_h)
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, merged_defs
         block.call
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, current_defs
       end
       doc name:          :with_merged_synth_defaults,
           introduced:    Version.new(2,0,0),
           summary:       "Block-level merge synth defaults ",
           doc:           "Specify synth arg values to be used by any following call to play within the specified do/end block. Merges the specified values with any previous defaults, rather than replacing them. After the do/end block has completed, previous defaults(if any) are restored. ",
           args:          [],
           opts:          {},
           accepts_block: true,
           examples:      ["
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
           doc:           "Specify new default values to be used by all subsequent calls to play. Will remove and override any previous defaults.",
           args:          [],
           opts:          {},
           accepts_block: false,
           examples:      ["
play 50 # plays note 50 with default arguments

use_synth_defaults amp: 0.5, cutoff: 70

play 50 # plays note 50 with an amp of 0.5, cutoff of 70 and defaults for rest of args

use_synth_defaults cutoff: 90

play 50 # plays note 50 with a cutoff of 70 and defaults for rest of args - note that amp is no longer 0.5
"]




       def with_synth_defaults(*args, &block)
         raise "with_synth_defaults must be called with a block" unless block
         current_defs = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults)

         args_h = resolve_synth_opts_hash_or_array(args)
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, args_h
         block.call
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, current_defs
       end
       doc name:          :with_synth_defaults,
           introduced:    Version.new(2,0,0),
           summary:       "Block-level use new synth defaults",
           doc:           "Specify new default values to be used by all calls to play within the do/end block. After the do/end block has completed the previous synth defaults (if any) are restored.",
           args:          [],
           opts:          {},
           accepts_block: true,
           examples:      ["
play 50 # plays note 50 with default arguments

use_synth_defaults amp: 0.5, pan: -1

play 50 # plays note 50 with an amp of 0.5, pan of -1 and defaults for rest of args

with_synth_defaults amp: 0.6, cutoff: 80
  play 50 # plays note 50 with an amp of 0.5, cutoff of 80 and defaults for rest of args (including pan)
end

play 60 # plays note 60 with an amp of 0.5, pan of -1 and defaults for rest of args
"]


       def use_fx(*args, &block)
         raise "use_fx isn't supported in this version of Sonic Pi. Perhaps you meant with_fx"
       end

       def with_fx(fx_name, *args, &block)
         raise "with_fx must be called with a block" unless block
         raise "with_fx block must only accept 0 or 1 args" unless [0, 1].include?(block.arity)

         ## Teach with_fx to do nothing if fx_name is :none
         if fx_name == :none
           if block.arity == 0
             return block.call
           else
             return block.call(@blank_node)
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

         __no_kill_block do
           ## Munge args
           args_h = resolve_synth_opts_hash_or_array(args)
           kill_delay = args_h[:kill_delay] || info.kill_delay(args_h)

           current_trackers = Thread.current.thread_variable_get(:sonic_pi_mod_sound_trackers) || Set.new

           ## We're still in a no_kill sync block, so the user can't
           ## kill us yet. Now that the gc thread is waiting for the fx
           ## block to either complete (or be killed) we can now set up
           ## the synth trackers, create the fx synth and busses and
           ## modify the thread local to make sure new synth triggers in
           ## this thread output to this fx synth:

           ## Create a new bus for this fx chain
           begin
             new_bus = @mod_sound_studio.new_fx_bus
           rescue AllocationError
             __delayed_serious_warning "All busses allocated - unable to honour FX"
             if block.arity == 0
               return block.call
             else
               return block.call(@blank_node)
             end
           end

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
               fx_synth.kill(true)
             end

             gc_completed.deliver! true
           end ## end gc collection thread definition

           ## Trigger new fx synth (placing it in the fx group) and
           ## piping the in and out busses correctly
           fx_synth = trigger_fx(fx_synth_name, args_h.merge({"in_bus" => new_bus}), current_fx_group)

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
         fx_execute_t = in_thread do
           Thread.current.thread_variable_set(:sonic_pi_spider_delayed_blocks, fxt.thread_variable_get(:sonic_pi_spider_delayed_blocks))
           Thread.current.thread_variable_set(:sonic_pi_spider_delayed_messages, fxt.thread_variable_get(:sonic_pi_spider_delayed_messages))

           new_trackers = [tracker]
           (Thread.current.thread_variable_get(:sonic_pi_mod_sound_trackers) || []).each do |tr|
             new_trackers << tr
           end
           Thread.current.thread_variable_set(:sonic_pi_mod_sound_trackers, new_trackers)

           begin
             if block.arity == 0
               block.call
             else
               block.call(fx_synth)
             end
           rescue
             ## Oopsey - there was an error in the user's block. Re-raise
             ## exception, but only after ensure block has executed.
             raise
           ensure
             ## Ensure that p's promise is delivered - thus kicking off
             ## the gc thread.
             p.deliver! true
             ## Reset out bus to value prior to this with_fx block
             fxt.thread_variable_set(:sonic_pi_mod_sound_synth_out_bus, current_bus)
           end
         end

         # Join thread used to execute block. Then transfer virtual
         # timestamp back to this thread.
         fx_execute_t.join
         Thread.current.thread_variable_set(:sonic_pi_spider_delayed_blocks, fx_execute_t.thread_variable_get(:sonic_pi_spider_delayed_blocks))
         Thread.current.thread_variable_set(:sonic_pi_spider_delayed_messages, fx_execute_t.thread_variable_get(:sonic_pi_spider_delayed_messages))
         Thread.current.thread_variable_set(:sonic_pi_spider_time, fx_execute_t.thread_variable_get(:sonic_pi_spider_time))

         # Wait for gc thread to complete. Once the gc thread has
         # completed, the tracker has been successfully removed, and all
         # the block threads have been determined. The gc thread has
         # spawned a new thread joining on those and waiting for all
         # remaining synths to complete and can be left to work in the
         # background...
         gc_completed.get
       end
       doc name:          :with_fx,
           introduced:    Version.new(2,0,0),
           summary:       "Use Studio FX",
           doc:           "This applies the named effect (FX) to everything within a given do/end block. Effects may take extra parameters to modify their behaviour. See FX help for parameter details.

For advanced control, it is also possible to modify the parameters of an effect within the body of the block. If you define the block with a single argument, the argument becomes a reference to the current effect and can be used to control its parameters (see examples).",
           args:          [[:fx_name, :symbol]],
           opts:          {},
           accepts_block: true,
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

  play 60 # plays note 50 with a little bit of reverb
  sleep 2

  control fx, mix: 0.5 # change the parameters of the effect to add more reverb
  play 60 # again note 60 but with more reverb
  sleep 2

  control fx, mix: 1 # change the parameters of the effect to add more reverb
  play 60 # plays note 60 with loads of reverb
  sleep 2
end"]




       def use_sample_pack(pack, &block)
         raise "use_sample_pack does not work with a block. Perhaps you meant with_sample_pack" if block
         pack = samples_path if pack == :default
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_sample_path, pack)
       end
       doc name:          :use_sample_pack,
           introduced:    Version.new(2,0,0),
           summary:       "Use sample pack",
           doc:           "Given a path to a folder of samples on your filesystem, this method makes any wav|wave|aif|aiff files in that folder available as samples. Consider using use_sample_pack_as when using multiple sample packs.",
           args:          [[:pack_path, :string]],
           opts:          nil,
           accepts_block: false,
           examples:      ["
use_sample_pack '/home/yourname/path/to/sample/dir'
sample :foo  #=> plays /home/yourname/path/to/sample/dir/foo.{wav|wave|aif|aiff}"]




       def use_sample_pack_as(pack, name, &block)
         raise "use_sample_pack_as does not work with a block. Perhaps you meant with_sample_pack" if block
         aliases = Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_aliases) || Hamster.hash
         new_aliases = aliases.put name.to_s, pack
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_sample_aliases, new_aliases)
       end
       doc name:          :use_sample_pack_as,
           introduced:    Version.new(2,0,0),
           summary:       "Use sample pack alias",
           doc:           "Similar to use_sample_pack except you can assign prefix aliases for samples. This lets you 'namespace' your sounds so that they don't clash, even if they have the same filename.",
           args:          [[:pack_path, :string]],
           opts:          nil,
           accepts_block: false,
           examples:      ["
# lets say you have two folders of your own sample files,
# and they both contain a file named 'bass.wav'
use_sample_pack_as '/home/yourname/my/cool/samples/guitar', :my_guitars
use_sample_pack_as '/home/yourname/my/cool/samples/drums', :my_drums

# You can now play both the 'bass.wav' samples, as they've had the symbol stuck on the front
sample :my_guitars_bass    #=> plays '/home/yourname/my/cool/samples/guitar/bass.wav'
sample :my_drums_bass  #=> plays '/home/yourname/my/cool/samples/drums/bass.wav'"]




       def with_sample_pack(pack, &block)
         raise "with_sample_pack requires a block. Perhaps you meant use_sample_pack" unless block
         pack = samples_path if pack == :default
         current = Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_path)
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_sample_path, pack)
         block.call
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_sample_path, current)
       end
       doc name:          :with_sample_pack,
           introduced:    Version.new(2,0,0),
           summary:       "Block-level use sample pack",
           doc:           "Given a path to a folder of samples on your filesystem, this method makes any wav|wave|aif|aiff files in that folder available as samples inside the given block. Consider using with_sample_pack_as when using multiple sample packs.",
           args:          [[:pack_path, :string]],
           opts:          nil,
           accepts_block: true,
           examples:      ["
with_sample_pack '/path/to/sample/dir' do
  sample :foo  #=> plays /path/to/sample/dir/foo.{wav|wave|aif|aiff}
end"]




       def with_sample_pack_as(pack, name, &block)
         raise "with_sample_pack_as requires a do/end block. Perhaps you meant use_sample_pack" if block
         current = Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_aliases)
         aliases = current || Hamster.hash
         new_aliases = aliases.put name.to_s, pack
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_sample_aliases, new_aliases)
         block.call
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_sample_aliases, current)
       end
       doc name:          :with_sample_pack_as,
           introduced:    Version.new(2,0,0),
           summary:       "Block-level use sample pack alias",
           doc:           "Similar to with_sample_pack except you can assign prefix aliases for samples. This lets you 'namespace' your sounds so that they don't clash, even if they have the same filename.",
           args:          [[:pack_path, :string]],
           opts:          nil,
           accepts_block: false,
           examples:      ["
with_sample_pack_as '/home/yourname/path/to/sample/dir', :my_samples do
  # The foo sample is now available, with a prefix of 'my_samples'
  sample :my_samples_foo  #=> plays /home/yourname/path/to/sample/dir/foo.{wav|wave|aif|aiff}
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
           doc:           "Returns the current debug setting (true or false).",
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
           doc:           "Returns the current arg checking setting (true or false).",
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
           doc:           "Set the main system volume to vol. Accepts a value between 0 and 5 inclusive. Vols greater or smaller than the allowed values are trimmed to keep them within range. Default is 1.",
           args:          [[:vol, :number]],
           opts:          nil,
           accepts_block: false,
           examples:      ["
set_volume! 2 # Set the main system volume to 2",

"set_volume! -1 # Out of range, so sets main system volume to 0",

"set_volume! 7 # Out of range, so sets main system volume to 5"
]




       def load_sample(path)
         case path
         when Symbol
           full_path = resolve_sample_symbol_path(path)
           raise "No sample exists called #{path.inspect}" unless File.exists?(full_path)
           info, cached = @mod_sound_studio.load_sample(full_path)
           __delayed_message "Loaded sample :#{path}" unless cached
           return info
         when String
           if File.exists?(path)
             info, cached = @mod_sound_studio.load_sample(path)
             __delayed_message "Loaded sample #{path.inspect}" unless cached
             return info
           else
             raise "No sample exists with path #{path}"
           end
         else
           raise "Unknown sample description: #{path}"
         end
       end
       doc name:          :load_sample,
           introduced:    Version.new(2,0,0),
           summary:       "Pre-load sample",
           doc:           "Given a path to a wav|wave|aif|aiff file, this loads the file and makes it available as a sample. See load_samples for loading multiple samples in one go.",
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
           doc:           "Given an array of paths to wav|wave|aif|aiff files, loads them all into memory so that they may be played with via sample with no delay. See load_sample.",
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
           doc:           "Alias for the load_sample method. Loads sample if necessary and returns sample information.",
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
           doc:           "Alias for the load_sample method. Loads sample if necessary and returns buffer information.",
           args:          [[:path, :string]],
           opts:          nil,
           accepts_block: false,
           examples:      ["see load_sample"]




       def sample_duration(path, *args)
         args_h = resolve_synth_opts_hash_or_array(args)
         args_h[:rate] = 1 unless args_h[:rate]
         load_sample(path).duration * 1.0/(args_h[:rate].abs)
       end
       doc name:          :sample_duration,
           introduced:    Version.new(2,0,0),
           summary:       "Get sample duration in seconds",
           doc:           "Given the name of a loaded sample, or a path to a wav|wave|aif|aiff file this returns the length of time that the sample would play for. It's useful when looping samples to make sure there are no gaps - see the examples. You may pass a rate opt which it will use to scale the returned time to match the duration at that rate.",
           args:          [[:path, :string]],
           opts:          {:rate => 1},
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
end "]




       def sample(path, *args_a_or_h)
         ensure_good_timing!
         buf_info = load_sample(path)
         args_h = resolve_synth_opts_hash_or_array(args_a_or_h)
         trigger_sampler path, buf_info.id, buf_info.num_chans, args_h
       end
       doc name:          :sample,
           introduced:    Version.new(2,0,0),
           summary:       "Trigger sample",
           doc:           "This is the main method for playing back recorded sound files (samples). Sonic Pi comes with lots of great samples included (see the section under help) but you can also load and play wav|wave|aif|aiff files from anywhere on your computer too. The 'rate' parameter affects both the speed and the pitch of the playback. See the examples for details. Check out the use_sample_pack and use_sample_pack_as methods for details on making it easy to work with a whole folder of your own sample files. Note, that on the first trigger of a sample, Sonic Pi has to load the sample which takes some time and may cause timing issues. To preload the samples you wish to work with consider load_sample and load_samples.",
           args:          [[:name_or_path, :symbol_or_string]],
           opts:          {:rate => 1, :attack => 0, :release => 0.0, :start => 0, :finish => 1, :pan => 0, :pan_slide => 0, :amp => 1, :amp_slide => 0},
           accepts_block: false,
           examples:      ["
sample :perc_bell # plays one of Sonic Pi's built in samples",
"sample '/home/yourname/path/to/a/sample.wav' # plays a wav|wave|aif|aiff file from your local filesystem",
"# Lets play with the rate parameter
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
         return nil if (n.nil? || n == :r || n == :rest)
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
           doc:           "Takes a midi note, a symbol (e.g. :C ) or a string (e.g. 'C' ) and resolves it to a midi note. You can also pass an optional :octave parameter to get the midi note for a given octave. Please note - :octave param is overridden if octave is specified in a symbol i.e. :c3. If the note is nil, :r or :rest, then nil is returned (nil represents a rest)",
           args:          [[:note, :symbol_or_number]],
           opts:          {:octave => 4},
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




       def note_info(n, *args)
         args_h = resolve_synth_opts_hash_or_array(args)
         octave = args_h[:octave]
         Note.new(n, octave)
       end
       doc name:          :note_info,
           introduced:    Version.new(2,0,0),
           summary:       "Get note info",
           doc:           "Returns an instance of SonicPi::Note. Please note - :octave param is overridden if octave is specified in a symbol i.e. :c3",
           args:          [[:note, :symbol_or_number]],
           opts:          {:octave => 4},
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
       doc:                "For a given scale and tonic it takes a symbol :i,:ii,:iii,:iv,:v :vi, :vii or a number 1-7 and resolves it to a midi note.",
       args:               [[:degree, :symbol_or_number], [:tonic, :symbol], [:scale, :symbol]],
       accepts_block:      false,
       examples:           [%Q{
play degree(:ii, :D3, :major)
play degree(2, :C3, :minor)
}]




       def scale(tonic, name, *opts)
         opts = resolve_synth_opts_hash_or_array(opts)
         opts = {:num_octaves => 1}.merge(opts)
         Scale.new(tonic, name,  opts[:num_octaves]).to_a
       end
       doc name:          :scale,
           introduced:    Version.new(2,0,0),
           summary:       "Create scale",
           doc:           "A helper method that returns an Array of midi note numbers when given a tonic note and a scale type. Also takes an optional :num_octaves parameter (1 octave is the default)",
           args:          [[:tonic, :symbol], [:name, :symbol]],
           opts:          {:num_octaves => 1},
           accepts_block: false,
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




       def chord(tonic, name=:major, *opts)
         if tonic.is_a? Array
           raise "List passed as parameter to chord needs two elements i.e. chord([:e3, :minor]), you passed: #{tonic.inspect}" unless tonic.size == 2
           Chord.new(tonic[0], tonic[1]).to_a
         else
           Chord.new(tonic, name).to_a
         end
       end
       doc name:          :chord,
           introduced:    Version.new(2,0,0),
           summary:       "Create chord",
           doc:           "A helper method that returns a list of midi note numbers when given a tonic note and a chord type",
           args:          [[:tonic, :symbol], [:name, :symbol]],
           opts:          nil,
           accepts_block: false,
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




       def control(node, *args)
         ensure_good_timing!
         args_h = resolve_synth_opts_hash_or_array(args)
         n = args_h[:note]
         args_h[:note] = note(n) if n
         node.control args_h
         unless Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_silent)
           __delayed_message "control node #{node.id}, #{arg_h_pp(args_h)}"
         end

       end
       doc name:          :control,
           introduced:    Version.new(2,0,0),
           summary:       "Control running synth",
           doc:           "Control a running synth node by passing new parameters to it. A synth node represents a running synth and can be obtained by assigning the return value of a call to play or sample or by specifying a parameter to the do/end block of an FX. You may modify any of the parameters you can set when triggering the synth, sample or FX. See documentation for parameter details.",
           args:          [[:node, :synth_node]],
           opts:          {},
           accepts_block: false,
           examples:      ["
my_node = play 50, release: 5, cutoff: 60 # play note 50 with release of 5 and cutoff of 60. Assign return value to variable my_node
sleep 1 # Sleep for a second
control my_node, cutoff: 70 # Now modify cutoff from 60 to 70, sound is still playing
sleep 1 # Sleep for another second
control my_node, cutoff: 90 # Now modify cutoff from 79 to 90, sound is still playing"]




       def stop(node)
         ensure_good_timing!
         alive = node.live?
         node.kill
         if alive
           __delayed_message "stopping sound #{node.id}"
         else
           __delayed_message "not stopping sound #{node.id} (already stopped)"
         end
       end
       doc name:          :stop,
           introduced:    Version.new(2,0,0),
           summary:       "Stop synth",
           doc:           "Stop a running synth sound or sample. In order to stop a sound, you need to have stored a reference to it in a variable.",
           args:          [[:node, :synth_node]],
           opts:          {},
           accepts_block: false,
           examples:      ["
# store a reference to a running synth in a variable called foo:
foo = play 50, release: 4
sleep 1
# foo is still playing, but we can stop it early:
stop foo
",
"bar = sample :loop_amen
sleep 0.5
stop bar"]




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




       def load_synthdefs(path)
         raise "No directory exists called #{path.inspect} " unless File.exists? path
         @mod_sound_studio.load_synthdefs(path)
         __info "Loaded synthdefs in path #{path}"
       end
       doc name:          :load_synthdefs,
           introduced:    Version.new(2,0,0),
           summary:       "Load external synthdefs",
           doc:           "Load all synth designs in the specified directory. This is useful if you wish to use your own SuperCollider synthesiser designs within Sonic Pi. If you wish your synth to seemlessly integrate with Sonic Pi's FX system you need to ensure your synth outputs a stereo signal to an audio bus with an index specified by a synth arg named out_bus.",
           args:          [[:path, :string]],
           opts:          nil,
           accepts_block: false,
           examples:      []




       private

       def scale_time_args_to_bpm(args_h, info)
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

       def resolve_sample_symbol_path(sym)
         if ((aliases = Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_aliases)) &&

             (m       = sym.to_s.match /(.+?)_(.+)/) &&
             (p       = aliases[m[1]]))
           path = p
           sym = m[2]
           partial = "#{p}/#{sym}"
         else
           path = Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_path) || samples_path
           partial = path + "/" + sym.to_s
         end

         ["wav", "aiff", "aif", "wave"].each do |ext|
           full = "#{partial}.#{ext}"
           return full if File.exists?(full)
         end

         raise "No sample exists called :#{sym} in sample pack #{path}"
       end

       def arg_h_pp(arg_h)
         s = "{"
         arg_h.each do |k, v|
           rounded = v.is_a?(Float) ? v.round(4) : v
           s << "#{k}: #{rounded}, "
         end
         s.chomp(", ") << "}"
       end

       def trigger_sampler(path, buf_id, num_chans, args_h, group=current_job_synth_group)
         args_h_with_buf = {:buf => buf_id}.merge(args_h)

         if (args_h[:rate] && args_h[:rate] < 0) || ((@complex_sampler_args - args_h.keys).size != @complex_sampler_args.size)
           synth_name = (num_chans == 1) ? "mono_player" : "stereo_player"
         else
           synth_name = (num_chans == 1) ? "basic_mono_player" : "basic_stereo_player"
         end
         sn = synth_name.to_sym
         info = SynthInfo.get_info(sn)

         unless Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_silent)
           if args_h.empty?
             __delayed_message "sample #{path.inspect}"
           else
             __delayed_message "sample #{path.inspect}, #{arg_h_pp(args_h)}"
           end
         end

         trigger_synth(synth_name, args_h_with_buf, group, info)
       end

       def trigger_inst(synth_name, args_h, group=current_job_synth_group)
         sn = synth_name.to_sym
         info = SynthInfo.get_info(sn)

         unless Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_silent)
           __delayed_message "synth #{sn.inspect}, #{arg_h_pp(args_h)}"
         end

         trigger_synth(synth_name, args_h, group, info)
       end

       def trigger_chord(synth_name, notes, args_a_or_h, group=current_job_synth_group)
         args_h = resolve_synth_opts_hash_or_array(args_a_or_h)

         chord_group = @mod_sound_studio.new_group(:tail, group)
         cg = ChordGroup.new(chord_group)

         nodes = []
         notes.each do |note|
           if note
             args_h[:note] = note
             nodes << trigger_inst(synth_name, args_h, cg)
           end
         end
         cg.sub_nodes = nodes
         cg
       end

       def trigger_fx(synth_name, args_a_or_h, group=current_fx_group)
         args_h = resolve_synth_opts_hash_or_array(args_a_or_h)
         sn = synth_name.to_sym
         info = SynthInfo.get_info(sn)

         n = trigger_synth(synth_name, args_h, group, info, true)

         info = SynthInfo.get_info(sn)
         FXNode.new(n, args_h["in_bus"], current_out_bus)
       end

       def trigger_synth(synth_name, args_h, group, info, now=false, out_bus=nil)

         defaults = info ? info.arg_defaults : {}
         synth_name = info ? info.scsynth_name : synth_name

         unless out_bus
           out_bus = current_out_bus
         end

         t_l_args = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults) || {}

         combined_args = defaults.merge(t_l_args.merge(args_h))
         combined_args = call_synth_default_fns(combined_args)
         combined_args["out_bus"] = out_bus

         validate_if_necessary! info, combined_args

         combined_args = scale_time_args_to_bpm(combined_args, info) if info && Thread.current.thread_variable_get(:sonic_pi_spider_arg_bpm_scaling)

         job_id = current_job_id
         __no_kill_block do

           p = Promise.new
           job_synth_proms_add(job_id, p)

           s = @mod_sound_studio.trigger_synth synth_name, group, combined_args, info, now

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

       def current_job_id
         Thread.current.thread_variable_get :sonic_pi_spider_job_id
       end

       def current_fx_group
         if g = Thread.current.thread_variable_get(:sonic_pi_mod_sound_fx_group)
           return g
         else
           g = job_fx_group(current_job_id)
           Thread.current.thread_variable_set :sonic_pi_mod_sound_fx_group, g
           return g
         end
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
             "in_bus" => job_bus(job_id),
             "amp" => 0.3
           }

           sn = "basic_mixer"
           info = SynthInfo.get_info(sn)
           defaults = info.arg_defaults
           synth_name = info ? info.scsynth_name : synth_name

           combined_args = defaults.merge(args_h)
           combined_args = call_synth_default_fns(combined_args)
           combined_args["out_bus"] = @mod_sound_studio.mixer_bus

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
           g = @mod_sound_studio.new_synth_group

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
           g = @mod_sound_studio.new_fx_group

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
           mixer.ctl amp: 0
           Kernel.sleep 0.5
           mixer.kill
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

       def call_synth_default_fns(args_h)
         args_h.each do |k, v|
           args_h[k] = v.call if v.is_a? Proc
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
     end
   end
 end
