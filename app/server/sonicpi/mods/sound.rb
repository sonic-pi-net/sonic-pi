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

module SonicPi
   module Mods
     module Sound

       include SonicPi::Util
       include SonicPi::DocSystem

       DEFAULT_PLAY_OPTS = {amp:       {default: 1, doc: "The amplitude of the note"},
                           amp_slide: {default: 0, doc: "The duration in seconds for amplitude changes to take place"},
                           pan:       {default: 0, doc: "The stereo position of the sound. -1 is left, 0 is in the middle and 1 is on the right. You may use value in between -1 and 1 such as 0.25"},
                           pan_slide: {default: 0, doc: "The duration in seconds for the pan value to change"},
                           attack: {default: :synth_specific, doc: "The duration in seconds for the sound to reach maximum amplitude. Choose short values for percusive sounds and long values for a fade-in effect."},
                           sustain: {default: 0, doc: "The duration in seconds for the sound to stay at full amplitude. Used to give the sound duration"},
                           release: {default: :synth_specific, doc: "The duration in seconds for the sound to fade out."}}

       def self.included(base)
         base.instance_exec {alias_method :sonic_pi_mods_sound_initialize_old, :initialize}

         base.instance_exec do
           define_method(:initialize) do |*splat, &block|
             sonic_pi_mods_sound_initialize_old *splat, &block
             hostname, port, msg_queue, max_concurrent_synths = *splat
             @complex_sampler_args = [:attack, :sustain, :release, :start, :end]

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
             @mod_sound_studio.sched_ahead_time = 0.5 if (os == :raspberry)

             @events.add_handler("/job-start", @events.gensym("/mods-sound-job-start")) do |payload|

               job_id = payload[:id]

               @job_proms_queues_mut.synchronize do
                 @job_proms_queues[job_id] = Queue.new
                 joiner = job_proms_joiner(job_id)
                 @job_proms_joiners[job_id] = joiner
               end

               t = payload[:thread]
               g = job_synth_group(job_id)
               t.thread_variable_set(:sonic_pi_mod_sound_synth_job_group, g)

               fx_g = job_fx_group(job_id)
               t.thread_variable_set(:sonic_pi_mod_sound_fx_group, fx_g)
             end

             @events.add_handler("/job-join", @events.gensym("/mods-sound-job-join")) do |payload|

               ## At this point we can be assured that no more threads
               ## are running for this particular job. We therefore
               ## don't have to worry about concurrency issues.
               job_id = payload[:id]

               joiner = @job_proms_joiners[job_id]
               @job_proms_queues[job_id] << :job_finished
               joiner.get
               @job_proms_queues_mut.synchronize do
                 @job_proms_joiners.delete job_id
               end
             end

             @events.add_handler("/job-completed", @events.gensym("/mods-sound-job-completed")) do |payload|
               job_id = payload[:id]
               Thread.new do

                 Thread.current.thread_variable_set(:sonic_pi_thread_group, "job_completed-#{job_id}")
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




       def set_sched_ahead_time!(t)
         @mod_sound_studio.sched_ahead_time = t
         __info "Schedule ahead time set to #{t}"
       end
       doc name:          :set_sched_ahead_time!,
           doc:           "Specify how many seconds ahead of time the synths should be triggered. This represents the amount of time between pressing 'Run' and hearing audio. A larger time gives the system more room to work with and can reduce performance issues in playing fast sections on slower platforms.",
           args:          [[:time, :number]],
           opts:          nil,
           accepts_block: false,
           examples:      ["set_sched_ahead_time! 1"]




       def use_debug(v, &block)
         raise "use_debug does not work with a do/end block. Perhaps you meant with_debug" if block
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_synth_silent, !v)
       end
       doc name:          :use_debug,
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
         @mod_sound_studio.current_synth_name = synth_name
       end
       doc name:          :use_synth,
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
         orig_synth = @mod_sound_studio.current_synth_name
         @mod_sound_studio.current_synth_name = synth_name
         block.call
         @mod_sound_studio.current_synth_name = orig_synth
       end
       doc name:          :with_synth,
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
         @tmp_path = File.expand_path("#{tmp_dir}/#{rand(100000000)}.wav")
         @mod_sound_studio.recording_start @tmp_path
       end
       doc name:          :recording_start,
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
           doc:           "add docs",
           args:          [],
           opts:          nil,
           accepts_block: false,
           examples:      [],
           hide:          true




       def play(n, *args)
         return play_chord(n, *args) if n.is_a?(Array)
         ensure_good_timing!

         if n
           n = note(n) unless n.is_a? Fixnum
           args_h = resolve_synth_opts_hash_or_array(args)
           if shift = Thread.current.thread_variable_get(:sonic_pi_mod_sound_transpose)
             n += shift
           end
           args_h[:note] = n unless args_h[:note]
           trigger_inst @mod_sound_studio.current_synth_name, args_h
         end
       end
       doc name:          :play,
           doc:           "Play note with current synth. Accepts a set of standard options which include control of an amplitude envelope with attack, sustain and release phases. These phases are triggered in order, so the duration of the sound is attack + sustain + release times. The duration of the sound does not affect any other notes. Code continues executing whilst the sound is playing through its envelope phases.

Accepts optional args for modification of the synth being played. See each synth's documentation for synth-specific opts. See use_synth and with_synth for changing the current synth.",
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
         notes.each_with_index{|note, idx| play(note, *args) ; sleep(times[idx % times.size])}
       end
       doc name:          :play_pattern_timed,
           doc:           "Play each note in a list of notes one after another with specified times between them. The notes should be a list of MIDI numbers or symbols such as :E4 - identical to the first parameter of the play function. The times should be a list of times between the notes in seconds.

If the list of times is smaller than the number of gaps between notes, the list is repeated again. If the list of times is longer than the number of gaps between notes, then some of the times are ignored. See examples for more detail.

Accepts optional args for modification of the synth being played. See each synth's documentation for synth-specific opts. See use_synth and with_synth for changing the current synth.",
           args:          [[:notes, :list], [:times, :list]],
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
         shifted_notes = notes.map{|n| n + shift}
         synth_name = @mod_sound_studio.current_synth_name
         trigger_chord(synth_name, shifted_notes, args)
       end
       doc name:          :play_chord,
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
         current_fns = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_default_fns)
         args_h = resolve_synth_opts_hash_or_array(args)
         defaults_h, fns_h = extract_synth_default_fns(args_h)
         merged_defs = (current_defs || {}).merge(defaults_h)
         merged_fns = (current_fns || {}).merge(fns_h)
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, merged_defs
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_default_fns, merged_fns
       end
       doc name:          :use_merged_synth_defaults,
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
         current_fns = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_default_fns)
         args_h = resolve_synth_opts_hash_or_array(args)
         defaults_h, fns_h = extract_synth_default_fns(args_h)
         merged_defs = (current_defs || {}).merge(defaults_h)
         merged_fns = (current_fns || {}).merge(fns_h)
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, merged_defs
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_default_fns, merged_fns
         block.call
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, current_defs
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults_fns, current_fns
       end
       doc name:          :with_merged_synth_defaults,
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
         defaults_h, fns_h = extract_synth_default_fns(args_h)
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_default_fns, fns_h
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, defaults_h
       end
       doc name:          :use_synth_defaults,
           doc:           "add docs",
           args:          [],
           opts:          {},
           accepts_block: false,
           examples:      []




       def with_synth_defaults(*args, &block)
         raise "with_synth_defaults must be called with a block" unless block
         current_defs = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults)
         current_fns = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_default_fns)
         args_h = resolve_synth_opts_hash_or_array(args)
         defaults_h, fns_h = extract_synth_default_fns(args_h)
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, defaults_h
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_default_fns, fns_h
         block.call
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, current_defs
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_default_fns, current_fns
       end
       doc name:          :with_synth_defaults,
           doc:           "add docs",
           args:          [],
           opts:          {},
           accepts_block: true,
           examples:      []




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
         t = in_thread do
           t.thread_variable_set(:sonic_pi_spider_delayed_blocks, fxt.thread_variable_get(:sonic_pi_spider_delayed_blocks))
           t.thread_variable_set(:sonic_pi_spider_delayed_messages, fxt.thread_variable_get(:sonic_pi_spider_delayed_messages))

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
         t.join
         Thread.current.thread_variable_set(:sonic_pi_spider_delayed_blocks, t.thread_variable_get(:sonic_pi_spider_delayed_blocks))
         Thread.current.thread_variable_set(:sonic_pi_spider_delayed_messages, t.thread_variable_get(:sonic_pi_spider_delayed_messages))
         Thread.current.thread_variable_set(:sonic_pi_spider_time, t.thread_variable_get(:sonic_pi_spider_time))

         # Wait for gc thread to complete. Once the gc thread has
         # completed, the tracker has been successfully removed, and all
         # the block threads have been determined. The gc thread has
         # spawned a new thread joining on those and waiting for all
         # remaining synths to complete and can be left to work in the
         # background...
         gc_completed.get
       end
       doc name:          :with_fx,
           doc:           "add docs",
           args:          [[:fx_name, :symbol]],
           opts:          {},
           accepts_block: true,
           examples:      []




       def use_sample_pack(pack, &block)
         raise "use_sample_pack does not work with a block. Perhaps you meant with_sample_pack" if block
         pack = samples_path if pack == :default
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_sample_path, pack)
       end
       doc name:          :use_sample_pack,
           doc:           "add docs",
           args:          [[:pack_path, :string]],
           opts:          nil,
           accepts_block: false,
           examples:      []




       def with_sample_pack(pack, &block)
         raise "with_sample_pack requires a block. Perhaps you meant use_sample_pack" unless block
         pack = samples_path if pack == :default
         current = Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_path)
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_sample_path, pack)
         block.call
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_sample_path, current)
       end
       doc name:          :with_sample_pack,
           doc:           "add docs",
           args:          [[:pack_path, :string]],
           opts:          nil,
           accepts_block: true,
           examples:      []




       def current_synth
         @mod_sound_studio.current_synth_name
       end
       doc name:          :current_synth,
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
           doc:           "Returns the current sample pack.",
           args:          [],
           opts:          nil,
           accepts_block: false,
           examples:      ["
puts current_sample_pack # Print out the current sample pack"]




       def current_synth_defaults
         defaults = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults)
         default_fns = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_default_fns)
         defaults.merge(default_fns)
       end
       doc name:          :current_synth_defaults,
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
           doc:           "Set the main system volum to vol. Accepts a value between 0 and 5 inclusive. Vols greater or smaller than the allowed values are trimmed to keep them within range. Default is 1.",
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
           doc:           "add docs",
           args:          [[:path, :string]],
           opts:          nil,
           accepts_block: false,
           examples:      []




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
           doc:           "add docs",
           args:          [[:paths, :list]],
           opts:          nil,
           accepts_block: false,
           examples:      []




       def sample_info(path)
         load_sample(path)
       end
       doc name:          :sample_info,
           doc:           "add docs",
           args:          [[:path, :string]],
           opts:          nil,
           accepts_block: false,
           examples:      []




       def sample_duration(path, *args)
         args_h = resolve_synth_opts_hash_or_array(args)
         args_h = {:rate => 1}.merge(args_h)
         load_sample(path).duration * 1.0/args_h[:rate]
       end
       doc name:          :sample_duration,
           doc:           "add docs",
           args:          [[:path, :string]],
           opts:          {:rate => 1},
           accepts_block: false,
           examples:      []




       def sample(path, *args_a_or_h)
         ensure_good_timing!
         buf_info = load_sample(path)
         args_h = resolve_synth_opts_hash_or_array(args_a_or_h)
         trigger_sampler path, buf_info.id, buf_info.num_chans, args_h
       end
       doc name:          :sample,
           doc:           "add docs",
           args:          [[:name_or_path, :symbol_or_string]],
           opts:          {:rate => 1},
           accepts_block: false,
           examples:      []




       def status
         @mod_sound_studio.status
       end
       doc name:          :status,
           doc:           "add docs",
           args:          [],
           opts:          nil,
           accepts_block: false,
           examples:      []




       def note(n, *args)
         Note.resolve_midi_note_without_octave(n) if args.empty?
         args_h = resolve_synth_opts_hash_or_array(args)
         octave = args_h[:octave]
         if octave
           Note.resolve_midi_note(n, octave)
         else
           Note.resolve_midi_note_without_octave(n)
         end
       end
       doc name:          :note,
           doc:           "add docs",
           args:          [[:note, :symbol_or_number]],
           opts:          {:octave => 4},
           accepts_block: false,
           examples:      []




       def note_info(n, *args)
         args_h = resolve_synth_opts_hash_or_array(args)
         octave = args_h[:octave]
         Note.new(n, octave)
       end
       doc name:          :note_info,
           doc:           "add docs - :octave opt is overridden if oct specified in symbol i.e. :c3",
           args:          [[:note, :symbol_or_number]],
           opts:          {:octave => 4},
           accepts_block: false,
           examples:      []




       def scale(tonic, name, *opts)
         opts = resolve_synth_opts_hash_or_array(opts)
         opts = {:num_octaves => 1}.merge(opts)
         Scale.new(tonic, name,  opts[:num_octaves]).to_a
       end
       doc name:          :scale,
           doc:           "add docs",
           args:          [[:tonic, :symbol], [:name, :symbol]],
           opts:          {:num_octaves => 1},
           accepts_block: false,
           examples:      []




       def chord(tonic, name=:major)
         if tonic.is_a? Array
           raise "Array passed as parameter to chord needs two elements i.e. chord([:e3, :minor]), you passed: #{tonic.inspect}" unless tonic.size == 2
           Chord.new(tonic[0], tonic[1]).to_a
         else
           Chord.new(tonic, name).to_a
         end
       end
       doc name:          :chord,
           doc:           "add docs",
           args:          [[:tonic, :symbol], [:name, :symbol]],
           opts:          nil,
           accepts_block: false,
           examples:      []




       def control(node, *args)
         ensure_good_timing!
         args_h = resolve_synth_opts_hash_or_array(args)
         n = args_h[:note]
         args_h[:note] = note(n) if n
         node.control args_h
         __delayed_message "control Node #{node.id}, #{arg_h_pp(args_h)}"
       end
       doc name:          :control,
           doc:           "add docs",
           args:          [[:node, :synth_node]],
           opts:          {},
           accepts_block: false,
           examples:      []




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
           doc:           "add docs",
           args:          [[:group, :symbol]],
           opts:          nil,
           accepts_block: false,
           examples:      []




       def all_sample_names
         BaseInfo.all_samples
       end
       doc name:          :all_sample_names,
           doc:           "add docs",
           args:          [],
           opts:          nil,
           accepts_block: false,
           examples:      []




       def sample_groups
         BaseInfo.grouped_samples.keys
       end
       doc name:          :sample_groups,
           doc:           "add docs",
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
           doc:           "add docs",
           args:          [[:path, :string]],
           opts:          nil,
           accepts_block: false,
           examples:      []

       private

       def resolve_sample_symbol_path(sym)
         path = Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_path) || samples_path
         partial = path + "/" + sym.to_s
         ["wav", "aiff", "aif", "wave"].each do |ext|
           full = "#{partial}.#{ext}"
           return full if File.exists?(full)
         end

         raise "No sample exists called #{path.inspect} in sample pack #{path}"
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

         validate_if_necessary! info, args_h_with_buf

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

         validate_if_necessary!(info, args_h)

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

         validate_if_necessary!(info, args_h)

         n = trigger_synth(synth_name, args_h, group, info, true)
         FXNode.new(n, args_h["in_bus"], current_out_bus)
       end

       def trigger_synth(synth_name, args_h, group, info, now=false, out_bus=nil)

         defaults = info ? info.arg_defaults : {}

         unless out_bus
           out_bus = current_out_bus
         end

         job_id = current_job_id
         t_l_args = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults) || {}
         t_l_fn_args = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_default_fns)

         if t_l_fn_args
           resolved_tl_fn_args = {}
           t_l_fn_args.each do |k, v|
             resolved_tl_fn_args[k] = v.call
           end
           combined_args = defaults.merge(resolved_tl_fn_args.merge(t_l_args).merge(args_h)).merge({"out_bus" => out_bus})
         else
           combined_args = defaults.merge(t_l_args.merge(args_h)).merge({"out_bus" => out_bus})
         end

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
         Thread.current.thread_variable_get :sonic_pi_mod_sound_fx_group
       end

       def current_job_synth_group
         Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_job_group)
       end

       def current_out_bus
         current_bus = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_out_bus)
         current_bus || @mod_sound_studio.mixer_bus
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
             "out_bus" => @mod_sound_studio.mixer_bus,
           }

           synth_name = "sp/basic_mixer"

           validation_fn = mk_synth_args_validator(synth_name)
           validation_fn.call(args_h)

           default_args = SynthInfo.get_info(synth_name).arg_defaults
           combined_args = default_args.merge(args_h)
           n = @mod_sound_studio.trigger_synth synth_name, job_fx_group(job_id), combined_args, true, &validation_fn

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
           mixer.ctl amp_slide: 0.2
           mixer.ctl amp: 0
           Kernel.sleep 0.2
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

       def extract_synth_default_fns(args_h)
         defaults = {}
         fns = {}
         args_h.each do |k, v|
           case v
           when Proc
             fns[k] = v
           else
             defaults[k] = v
           end
         end
         [defaults, fns]
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
         if Thread.current.thread_variable_get(:sonic_pi_mod_sound_check_synth_args)
           info.should_validate = true
           info.validate!(args_h)
         else
           info.should_validate = false
         end
       end

       def ensure_good_timing!
         vt = Thread.current.thread_variable_get :sonic_pi_spider_time
         sat = @mod_sound_studio.sched_ahead_time + 0.1
         now = Time.now
         raise "Timing Exception: thread got too far behind time." if (now - (sat + 1)) > vt
       end

     end
   end
 end
