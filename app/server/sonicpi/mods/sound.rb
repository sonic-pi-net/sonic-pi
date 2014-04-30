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

       def self.included(base)
         base.instance_exec {alias_method :sonic_pi_mods_sound_initialize_old, :initialize}

         base.instance_exec do
           define_method(:initialize) do |*splat, &block|
             sonic_pi_mods_sound_initialize_old *splat, &block
             hostname, port, msg_queue, max_concurrent_synths = *splat
             @complex_sampler_args = [:attack, :sustain, :release, :start, :end]
             @blank_node = BlankNode.new
             @JOB_SYNTH_PROMS_A = Atom.new(Hamster.hash)
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
             @events.add_handler("/job-join", @events.gensym("/mods-sound-job-join")) do |payload|
               job_id = payload[:id]
               (@JOB_SYNTH_PROMS_A.deref[job_id] || []).each do |csp|
                 csp.get
               end
             end

             @events.add_handler("/job-completed", @events.gensym("/mods-sound-job-completed")) do |payload|
               job_id = payload[:id]
               job_t = payload[:thread]

               @JOB_SYNTH_PROMS_A.swap! do |ps|
                 ps.delete job_id
               end
               Thread.new do
                 Thread.current.thread_variable_set(:sonic_pi_thread_group, :job_completed)
                 Thread.current.priority = -1
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

       doc name:          :set_sched_ahead_time!,
           doc:           "add docs",
           args:          [[:time, :number]],
           opts:          nil,
           accepts_block: false,
           examples:      []
       def set_sched_ahead_time!(t)
         @mod_sound_studio.sched_ahead_time = t
       end

       doc name:          :use_debug,
           doc:           "add docs",
           args:          [[:true_or_false, :boolean]],
           opts:          nil,
           accepts_block: false,
           examples:      []
       def use_debug(v, &block)
         raise "use_debug does not work with a do/end block. Perhaps you meant with_debug" if block
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_synth_silent, !v)
       end

       doc name:          :with_debug,
           doc:           "add docs",
           args:          [[:true_or_false, :boolean]],
           opts:          nil,
           accepts_block: true,
           examples:      []
       def with_debug(v, &block)
         raise "with_debug requires a do/end block. Perhaps you meant use_debug" unless block
         current = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_silent)
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_synth_silent, !v)
         block.call
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_synth_silent, current)
       end

       doc name:          :use_arg_checks,
           doc:           "add docs",
           args:          [[:true_or_false, :boolean]],
           opts:          nil,
           accepts_block: false,
           examples:      []
       def use_arg_checks(v, &block)
         raise "use_arg_checks does not work with a a do/end block. Perhaps you meant use_arg_checks" if block

         Thread.current.thread_variable_set(:sonic_pi_mod_sound_check_synth_args, v)
       end

       doc name:          :with_arg_checks,
           doc:           "add docs",
           args:          [[:true_or_false, :boolean]],
           opts:          nil,
           accepts_block: true,
           examples:      []
       def with_arg_checks(v, &block)
         raise "with_arg_checks requires a do/end block. Perhaps you meant use_arg_checks" unless block

         current = Thread.current.thread_variable_get(:sonic_pi_mod_sound_check_synth_args)
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_check_synth_args, v)
         block.call
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_check_synth_args, current)
       end

       doc name:          :use_transpose,
           doc:           "add docs",
           args:          [[:note_shift, :number]],
           opts:          nil,
           accepts_block: false,
           examples:      []
       def use_transpose(shift, &block)
         raise "use_transpose does not work with a do/end block. Perhaps you meant with_transpose" if block
         raise "Transpose value must be a number, got #{shift.inspect}" unless shift.is_a?(Fixnum)
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_transpose, shift)
       end

       doc name:          :with_transpose,
           doc:           "add docs",
           args:          [[:note_shift, :number]],
           opts:          nil,
           accepts_block: true,
           examples:      []
       def with_transpose(shift, &block)
         raise "with_transpose requires a do/end block. Perhaps you meant use_transpose" unless block
         raise "Transpose value must be a number, got #{shift.inspect}" unless shift.is_a?(Fixnum)
         curr = Thread.current.thread_variable_get(:sonic_pi_mod_sound_transpose)
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_transpose, shift)
         block.call
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_transpose, curr)
       end

       doc name:          :use_synth,
           doc:           "add docs",
           args:          [[:synth_name, :symbol]],
           opts:          nil,
           accepts_block: false,
           examples:      []
       def use_synth(synth_name, &block)
         raise "use_synth does not work with a do/end block. Perhaps you meant with_synth" if block
         @mod_sound_studio.current_synth_name = synth_name
       end

       doc name:          :with_synth,
           doc:           "add docs",
           args:          [[:synth_name, :symbol]],
           opts:          nil,
           accepts_block: true,
           examples:      []
       def with_synth(synth_name, &block)
         raise "with_synth must be called with a do/end block. Perhaps you meant use_synth" unless block
         orig_synth = @mod_sound_studio.current_synth_name
         @mod_sound_studio.current_synth_name = synth_name
         block.call
         @mod_sound_studio.current_synth_name = orig_synth
       end

       doc name:          :recording_start,
           doc:           "add docs",
           args:          [],
           opts:          nil,
           accepts_block: false,
           examples:      []
       def recording_start
         puts "start recording"
         tmp_dir = Dir.mktmpdir("sonic-pi")
         @tmp_path = File.expand_path("#{tmp_dir}/#{rand(100000000)}.wav")
         puts "tmp_path: #{@tmp_path}"
         @mod_sound_studio.recording_start @tmp_path
       end

       doc name:          :recording_stop,
           doc:           "add docs",
           args:          [],
           opts:          nil,
           accepts_block: false,
           examples:      []
       def recording_stop
         puts "stop recording"
         @mod_sound_studio.recording_stop
       end

       doc name:          :recording_save,
           doc:           "add docs",
           args:          [[:path, :string]],
           opts:          nil,
           accepts_block: false,
           examples:      []
       def recording_save(filename)
         puts "save recording #{filename}"
         Kernel.sleep 3
         FileUtils.mv(@tmp_path, filename)
         @tmp_path = nil
       end

       doc name:          :recording_delete,
           doc:           "add docs",
           args:          [],
           opts:          nil,
           accepts_block: false,
           examples:      []
       def recording_delete
         puts "delete recording"
         FileUtils.rm @tmp_path if @tmp_path
       end


       doc name:          :play,
           doc:           "add docs",
           args:          [[:note, :symbol_or_number]],
           opts:          {},
           accepts_block: false,
           examples:      []
       def play(n, *args)
         return play_chord(n, *args) if n.is_a?(Array)

         if n
           n = note(n) unless n.is_a? Fixnum
           args_h = resolve_synth_opts_hash_or_array(args)
           shift = Thread.current.thread_variable_get(:sonic_pi_mod_sound_transpose) || 0
           shifted_n = n + shift
           args_h = {:note => shifted_n}.merge(args_h)
           trigger_inst @mod_sound_studio.current_synth_name, args_h
         end
       end

       doc name:          :play_pattern,
           doc:           "add docs",
           args:          [[:notes, :list]],
           opts:          {},
           accepts_block: false,
           examples:      []
       def play_pattern(notes, *args)
         notes.each{|note| play(note, *args) ; sleep 1 }
       end

       doc name:          :play_pattern_timed,
           doc:           "add docs",
           args:          [[:notes, :list], [:times, :list]],
           opts:          {},
           accepts_block: false,
           examples:      []
       def play_pattern_timed(notes, times, *args)
         notes.each_with_index{|note, idx| play(note, *args) ; sleep(times[idx % times.size])}
       end

       doc name:          :play_chord,
           doc:           "add docs",
           args:          [[:notes, :list]],
           opts:          {},
           accepts_block: false,
           examples:      []
       def play_chord(notes, *args)
         shift = Thread.current.thread_variable_get(:sonic_pi_mod_sound_transpose) || 0
         shifted_notes = notes.map{|n| n + shift}
         synth_name = @mod_sound_studio.current_synth_name
         trigger_chord(synth_name, shifted_notes, args)
       end

       doc name:          :repeat,
           doc:           "add docs",
           args:          [[:notes, :list]],
           opts:          {},
           accepts_block: false,
           examples:      []
       def repeat(&block)
         while true
           block.call
         end
       end

       doc name:          :use_merged_synth_defaults,
           doc:           "add docs",
           args:          [],
           opts:          {},
           accepts_block: false,
           examples:      []
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

       doc name:          :with_merged_synth_defaults,
           doc:           "add docs",
           args:          [],
           opts:          {},
           accepts_block: true,
           examples:      []
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

       doc name:          :use_synth_defaults,
           doc:           "add docs",
           args:          [],
           opts:          {},
           accepts_block: false,
           examples:      []
       def use_synth_defaults(*args, &block)
         raise "use_synth_defaults does not work with a block. Perhaps you meant with_synth_defaults" if block
         args_h = resolve_synth_opts_hash_or_array(args)
         defaults_h, fns_h = extract_synth_default_fns(args_h)
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_default_fns, fns_h
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, defaults_h
       end

       doc name:          :with_synth_defaults,
           doc:           "add docs",
           args:          [],
           opts:          {},
           accepts_block: true,
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

       doc name:          :with_fx,
           doc:           "add docs",
           args:          [[:fx_name, :symbol]],
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

         start_subthreads = []
         end_subthreads = []

         fx_synth_name = "fx_#{fx_name}"

         fxt = Thread.current
         p = Promise.new
         gc_completed = Promise.new

         # These will be assigned later...
         fx_synth = BlankNode.new
         new_bus = nil
         current_bus = nil
         tracker = nil

         __no_kill_block do
           ## Munge args
           args_h = resolve_synth_opts_hash_or_array(args)
           kill_delay = args_h[:kill_delay] || SynthInfo.get_info(fx_synth_name).kill_delay(args_h)

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
             __message "All busses allocated - unable to honour FX"
             if block.arity == 0
               return block.call
             else
               return block.call(@blank_node)
             end
           end

           gc = Thread.new do
             Thread.current.thread_variable_set(:sonic_pi_thread_group, :gc)
             Thread.current.priority = -1
             ## Need to block until either the thread died (which will be
             ## if the job was stopped whilst this fx block was being
             ## executed or if the fx block has completed.
             fx_completed = Promise.new

             t1 = Thread.new do
               Thread.current.thread_variable_set(:sonic_pi_thread_group, :gc_parent_join)
               Thread.current.priority = -1
               fxt.join
               ## Parent thread died - user must have stopped
               fx_completed.deliver! true
             end

             t2 = Thread.new do
               Thread.current.thread_variable_set(:sonic_pi_thread_group, :gc_fx_block_join)
               Thread.current.priority = -1
               p.get
               ## FX block completed
               fx_completed.deliver! true
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
               Thread.current.priority = -1
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
           fx_synth = trigger_fx(fx_synth_name, args_h.merge({"in-bus" => new_bus}), current_fx_group)

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
         Thread.current.thread_variable_set(:sonic_pi_spider_time, t.thread_variable_get(:sonic_pi_spider_time))

         # Wait for gc thread to complete. Once the gc thread has
         # completed, the tracker has been successfully removed, and all
         # the block threads have been determined. The gc thread has
         # spawned a new thread joining on those and waiting for all
         # remaining synths to complete and can be left to work in the
         # background...
         gc_completed.get
       end


       doc name:          :use_sample_pack,
           doc:           "add docs",
           args:          [[:pack_path, :string]],
           opts:          nil,
           accepts_block: false,
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
           accepts_block: true,
           examples:      []
       def with_sample_pack(pack, &block)
         raise "with_sample_pack requires a block. Perhaps you meant use_sample_pack" unless block
         pack = samples_path if pack == :default
         current = Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_path)
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_sample_path, pack)
         block.call
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_sample_path, current)
       end

       doc name:          :current_bpm,
           doc:           "add docs",
           args:          [],
           opts:          nil,
           accepts_block: false,
           examples:      []
       def current_bpm
         60.0 / Thread.current.thread_variable_get(:sonic_pi_sleep_mul)
       end

       doc name:          :set_debug_on!,
           doc:           "add docs",
           args:          [],
           opts:          nil,
           accepts_block: false,
           examples:      [],
           hide:          true
       def set_debug_on!
         @mod_sound_studio.debug = true
       end

       doc name:          :set_debug_off!,
           doc:           "add docs",
           args:          [],
           opts:          nil,
           accepts_block: false,
           examples:      [],
           hide:          true
       def set_debug_off!
         @mod_sound_studio.debug = false
       end

       doc name:          :set_volume!,
           doc:           "add docs",
           args:          [[:vol, :number]],
           opts:          nil,
           accepts_block: false,
           examples:      []
       def set_volume!(vol)
         max_vol = 5
         if (vol < 0)
           @mod_sound_studio.volume = 0
         elsif (vol > max_vol)
           @mod_sound_studio.volume = max_vol
         else
           @mod_sound_studio.volume = vol
         end
       end


       def resolve_sample_symbol_path(sym)
         path = Thread.current.thread_variable_get(:sonic_pi_mod_sound_sample_path) || samples_path
         partial = path + "/" + sym.to_s
         ["wav", "aiff", "aif", "wave"].each do |ext|
           full = "#{partial}.#{ext}"
           return full if File.exists?(full)
         end

         raise "No sample exists called #{path.inspect} in sample pack #{path}"
       end

       doc name:          :load_sample,
           doc:           "add docs",
           args:          [[:path, :string]],
           opts:          nil,
           accepts_block: false,
           examples:      []
       def load_sample(path)
         case path
         when Symbol
           full_path = resolve_sample_symbol_path(path)
           raise "No sample exists called #{path.inspect}" unless File.exists?(full_path)
           info, cached = @mod_sound_studio.load_sample(full_path)
           puts "Loaded sample :#{path}" unless cached
           return info
         when String
           if File.exists?(path)
             info, cached = @mod_sound_studio.load_sample(path)
             puts "Loaded sample #{path.inspect}" unless cached
             return info
           else
             raise "No sample exists with path #{path}"
           end
         else
           raise "Unknown sample description: #{path}"
         end
       end

       doc name:          :load_samples,
           doc:           "add docs",
           args:          [[:paths, :list]],
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

       doc name:          :sample_info,
           doc:           "add docs",
           args:          [[:path, :string]],
           opts:          nil,
           accepts_block: false,
           examples:      []
       def sample_info(path)
         load_sample(path)
       end

       doc name:          :sample_duration,
           doc:           "add docs",
           args:          [[:path, :string]],
           opts:          {:rate => 1},
           accepts_block: false,
           examples:      []
       def sample_duration(path, *args)
         args_h = resolve_synth_opts_hash_or_array(args)
         args_h = {:rate => 1}.merge(args_h)
         load_sample(path).duration * 1.0/args_h[:rate]
       end


       doc name:          :sample,
           doc:           "add docs",
           args:          [[:name_or_path, :symbol_or_string]],
           opts:          {:rate => 1},
           accepts_block: false,
           examples:      []
       def sample(path, *args_a_or_h)
         buf_info = load_sample(path)
         args_h = resolve_synth_opts_hash_or_array(args_a_or_h)
         trigger_sampler path, buf_info.id, buf_info.num_chans, args_h
       end

       doc name:          :status,
           doc:           "add docs",
           args:          [],
           opts:          nil,
           accepts_block: false,
           examples:      []
       def status
         @mod_sound_studio.status
       end

       doc name:          :note,
           doc:           "add docs",
           args:          [[:note, :symbol_or_number]],
           opts:          {:octave => 4},
           accepts_block: false,
           examples:      []
       def note(n, *args)
         args_h = resolve_synth_opts_hash_or_array(args)
         octave = args_h[:octave]
         Note.resolve_midi_note(n, octave)
       end

       doc name:          :note_info,
           doc:           "add docs - :octave opt is overridden if oct specified in symbol i.e. :c3",
           args:          [[:note, :symbol_or_number]],
           opts:          {:octave => 4},
           accepts_block: false,
           examples:      []
       def note_info(n, *args)
         args_h = resolve_synth_opts_hash_or_array(args)
         octave = args_h[:octave]
         Note.new(n, octave)
       end

       doc name:          :scale,
           doc:           "add docs",
           args:          [[:tonic, :symbol], [:name, :symbol]],
           opts:          {:num_octaves => 1},
           accepts_block: false,
           examples:      []
       def scale(tonic, name, *opts)
         opts = resolve_synth_opts_hash_or_array(opts)
         opts = {:num_octaves => 1}.merge(opts)
         Scale.new(tonic, name,  opts[:num_octaves]).to_a
       end

       doc name:          :chord,
           doc:           "add docs",
           args:          [[:tonic, :symbol], [:name, :symbol]],
           opts:          nil,
           accepts_block: false,
           examples:      []
       def chord(tonic, name)
         Chord.new(tonic, name).to_a
       end


       doc name:          :control,
           doc:           "add docs",
           args:          [[:node, :synth_node]],
           opts:          {},
           accepts_block: false,
           examples:      []
       def control(node, *args)
         node.control *args
       end

       doc name:          :sample_names,
           doc:           "add docs",
           args:          [[:group, :symbol]],
           opts:          nil,
           accepts_block: false,
           examples:      []
       def sample_names(group)
         BaseInfo.grouped_samples[group][:samples]
       end

       doc name:          :all_sample_names,
           doc:           "add docs",
           args:          [],
           opts:          nil,
           accepts_block: false,
           examples:      []
       def all_sample_names
         BaseInfo.all_samples
       end

       doc name:          :sample_groups,
           doc:           "add docs",
           args:          [],
           opts:          nil,
           accepts_block: false,
           examples:      []
       def sample_groups
         BaseInfo.grouped_samples.keys
       end

       private

       def arg_h_pp(arg_h)
         s = "{"
         arg_h.each do |k, v|
           s << "#{k}: #{v}, "
         end
         s.chomp(", ") << "}"
       end

       def mk_synth_args_validator(synth_name)
         # It feelss messed up that I need the following line, but if I
         # don't use it, then synth_name within the lambda can be
         # changed externally affecting the internal lexical
         # representation.
         sn = synth_name
         arg_validation_fn = lambda do |args|
           args = munge_synth_args(args)
           info = SynthInfo.get_info(sn)
           raise "Unable to find synth info for #{sn}" unless info
           info.validate!(args)
           args
         end
         arg_validation_fn
       end

       def trigger_sampler(path, buf_id, num_chans, args_h, group=current_job_synth_group)
         args_h_with_buf = {:buf => buf_id}.merge(args_h)

         if (args_h[:rate] && args_h[:rate] < 0) || ((@complex_sampler_args - args_h.keys).size != @complex_sampler_args.size)
           synth_name = (num_chans == 1) ? "mono_player" : "stereo_player"
         else
           synth_name = (num_chans == 1) ? "basic_mono_player" : "basic_stereo_player"
         end

         if Thread.current.thread_variable_get(:sonic_pi_mod_sound_check_synth_args)
           validation_fn = mk_synth_args_validator(synth_name)
           validation_fn.call(args_h_with_buf)
         end

         unless Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_silent)
           if args_h.empty?
             __delayed_message "Playing sample #{path.inspect}"
           else
             __delayed_message "Playing sample #{path.inspect} with: #{arg_h_pp(args_h)}"
           end
         end

         trigger_synth(synth_name, args_h_with_buf, group, validation_fn)
       end

       def trigger_inst(synth_name, args_h, group=current_job_synth_group)

         if Thread.current.thread_variable_get(:sonic_pi_mod_sound_check_synth_args)
           validation_fn = mk_synth_args_validator(synth_name)
           validation_fn.call(args_h)
         end

         unless Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_silent)
           __delayed_message "Playing #{synth_name} with: #{arg_h_pp(args_h)}"
         end
         trigger_synth(synth_name, args_h, group, validation_fn)
       end

       def trigger_chord(synth_name, notes, args_a_or_h, group=current_job_synth_group)
         args_h = resolve_synth_opts_hash_or_array(args_a_or_h)

         if Thread.current.thread_variable_get(:sonic_pi_mod_sound_check_synth_args)
           validation_fn = mk_synth_args_validator(synth_name)
           validation_fn.call(args_h)
         end

         chord_group = @mod_sound_studio.new_group(:tail, group)
         cg = ChordGroup.new(chord_group)

         nodes = []
         notes.each do |note|
           if note
             note_args_h = args_h.merge({:note => note})
             nodes << trigger_inst(synth_name, note_args_h, cg)
           end
         end
         cg.sub_nodes = nodes
         cg
       end

       def trigger_fx(synth_name, args_a_or_h, group=current_fx_group)
         args_h = resolve_synth_opts_hash_or_array(args_a_or_h)

         if Thread.current.thread_variable_get(:sonic_pi_mod_sound_check_synth_args)
           validation_fn = mk_synth_args_validator(synth_name)
           validation_fn.call(args_h)
         end

         n = trigger_synth(synth_name, args_h, group, validation_fn, true)
         FXNode.new(n, args_h["in-bus"], current_out_bus)
       end

       def trigger_synth(synth_name, args_h, group, arg_validation_fn, now=false, out_bus=nil)
         info = SynthInfo.get_info(synth_name)

         defaults = info ? info.arg_defaults : {}

         synth_name = "sp/#{synth_name}"
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
           combined_args = defaults.merge(resolved_tl_fn_args.merge(t_l_args).merge(args_h)).merge({"out-bus" => out_bus})
         else
           combined_args = defaults.merge(t_l_args.merge(args_h)).merge({"out-bus" => out_bus})
         end

         __no_kill_block do

           p = Promise.new
           job_synth_proms_add(job_id, p)

           s = @mod_sound_studio.trigger_synth synth_name, group, combined_args, now, &arg_validation_fn

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
         job_fx_group(current_job_id)
       end

       def current_job_synth_group
         job_synth_group(current_job_id)
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
             "in-bus" => job_bus(job_id),
             "out-bus" => @mod_sound_studio.mixer_bus,
           }

           synth_name = :basic_mixer

           validation_fn = mk_synth_args_validator(synth_name)
           validation_fn.call(args_h)

           default_args = SynthInfo.get_info(synth_name).arg_defaults
           combined_args = default_args.merge(args_h)
           n = @mod_sound_studio.trigger_synth "sp/#{synth_name}", job_fx_group(job_id), combined_args, true, &validation_fn

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
         @JOB_SYNTH_PROMS_A.swap! do |js|
           proms = js[job_id] || Hamster.set
           proms = proms.add p
           js.put job_id, proms
         end
       end

       def job_synth_proms_rm(job_id, p)
         @JOB_SYNTH_PROMS_A.swap! do |js|
           proms = js[job_id] || Hamster.set
           proms = proms.delete p
           js.put job_id, proms
         end
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

       def munge_synth_args(args)
         args = Hash[*args] if args.is_a? Array
         # ensure note is a MIDI note
         n = args[:note]
         args[:note] = note(n) if n

         args
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

       def __delayed_message(m)
         ## TODO: register this thread so that it's killed when job is
         ## killed. Using in_thread won't work - needs to be a different
         ## mechanism.
         Thread.new do
           Thread.current.thread_variable_set(:sonic_pi_thread_group, :delayed_message)
           Thread.current.priority = -1
           Kernel.sleep @mod_sound_studio.sched_ahead_time
           __message m
         end
       end
     end
   end
 end
