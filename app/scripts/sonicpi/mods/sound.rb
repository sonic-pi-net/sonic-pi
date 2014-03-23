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
require 'thread'
require "hamster/set"
require_relative "../blanknode"
require_relative "../note"
require_relative "../scale"
require_relative "../chord"
require_relative "../chordgroup"
require_relative "../synthtracker"

module SonicPi
   module Mods
     module Sound

       include SonicPi::Util

       def self.included(base)
         base.instance_exec {alias_method :sonic_pi_mods_sound_initialize_old, :initialize}

         base.instance_exec do
           define_method(:initialize) do |*splat, &block|
             sonic_pi_mods_sound_initialize_old *splat, &block
             hostname, port, msg_queue, max_concurrent_synths = *splat
             @blank_node = BlankNode.new
             @JOB_SYNTH_PROMS_A = Atom.new(Hamster.hash)
             @JOB_GROUPS_A = Atom.new(Hamster.hash)
             @JOB_GROUP_MUTEX = Mutex.new
             @JOB_FX_GROUPS_A = Atom.new(Hamster.hash)
             @JOB_FX_GROUP_MUTEX = Mutex.new
             @mod_sound_studio = Studio.new(hostname, port, msg_queue, max_concurrent_synths)
             @mod_sound_studio.sched_ahead_time(0.5) if (os == :raspberry)
             @events.add_handler("/job-join", @events.gensym("/mods-sound-job-join")) do |payload|

               job_id = payload[:id]

               (@JOB_SYNTH_PROMS_A.deref[job_id] || []).each do |csp|
                 csp.get
               end
             end

             @events.add_handler("/job-completed", @events.gensym("/mods-sound-job-completed")) do |payload|
               job_id = payload[:id]

               @JOB_SYNTH_PROMS_A.swap! do |ps|
                 ps.delete job_id
               end

               kill_job_group(job_id)
               kill_fx_job_group(job_id)

             end

             @events.add_handler("/exit", @events.gensym("/mods-sound-exit")) do |payload|
               @mod_sound_studio.exit
             end
           end
         end
       end

       def set_sched_ahead_time!(t)
         @mod_sound_studio.sched_ahead_time(t)
       end

       def use_synth(synth_name, &block)
         raise "use_synth does not work with a block. Perhaps you meant with_synth" if block
         @mod_sound_studio.current_synth_name = synth_name
       end

       def with_synth(synth_name, &block)
         raise "with_synth must be called with a block" unless block
         orig_synth = @mod_sound_studio.current_synth_name
         @mod_sound_studio.current_synth_name = synth_name
         block.call
         @mod_sound_studio.current_synth_name = orig_synth
       end

       def play(n, *args)
         return play_chord(n, *args) if n.is_a?(Array)

         if n
           n = note(n)
           args_h = resolve_synth_opts_hash_or_array(args)

           args_h = {:note => n}.merge(args_h)
           trigger_inst @mod_sound_studio.current_synth_name, args_h
         end
       end

       def play_pattern(notes, *args)
         notes.each{|note| play(note, *args) ; sleep(@mod_sound_studio.beat_s)}
       end

       def play_pattern_timed(notes, times, *args)
         notes.each_with_index{|note, idx| play(note, *args) ; sleep(times[idx % times.size])}
       end

       def play_chord(notes, *args)
         synth_name = @mod_sound_studio.current_synth_name
         trigger_chord(synth_name, notes, args)
       end

       def repeat(&block)
         while true
           block.call
         end
       end

       def use_merged_synth_defaults(*args, &block)
         raise "use_merged_synth_defaults does not work with a block. Perhaps you meant with_merged_synth_defaults" if block
         current = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults)
         args_h = resolve_synth_opts_hash_or_array(args)
         merged = (current || {}).merge(args_h)
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, merged
       end

       def with_merged_synth_defaults(*args, &block)
         raise "with_merged_synth_defaults must be called with a block" unless block
         current = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults)
         args_h = resolve_synth_opts_hash_or_array(args)
         merged = (current || {}).merge(args_h)
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, merged
         block.call
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, current
       end

       def use_synth_defaults(*args, &block)
         raise "use_synth_defaults does not work with a block. Perhaps you meant with_synth_defaults" if block
         new = resolve_synth_opts_hash_or_array(args)
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, new
       end

       def with_synth_defaults(*args, &block)
         raise "with_synth_defaults must be called with a block" unless block
         current = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults)
         new = resolve_synth_opts_hash_or_array(args)
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, new
         block.call
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, current
       end

       def with_fx(fx_name, *args, &block)
         raise "with_fx must be called with a block" unless block
         raise "with_fx block must only accept 0 or 1 args" unless [0, 1].include?(block.arity)

         ## Create a new bus for this fx chain
         begin
           new_bus = @mod_sound_studio.new_fx_bus
         rescue BusAllocationError
           __message "All busses allocated - unable to honour FX"
           if block.arity == 0
             return block.call
           else
             return block.call(@blank_node)
           end
         end

         ## Munge args
         args_h = resolve_synth_opts_hash_or_array(args)
         fx_synth_name = "fx_#{fx_name}"

         ## TODO:
         ## Do something more smart than just blindly use a magic 10s
         kill_delay = args_h[:kill_delay] || 1

         ## Get this thread's out bus (defaulting to the mixer if this thread hasn't got one)
         current_bus = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_out_bus)
         out_bus = current_bus || @mod_sound_studio.mixer_bus


         ## Trigger new fx synth piping the in and out busses correctly
         fx_synth = trigger_fx(fx_synth_name, args_h.merge({"in-bus" => new_bus, "out-bus" => out_bus}), current_fx_group)

         ## Get list of current subthreads
         start_subthreads = []
         end_subthreads = []
         Thread.current.thread_variable_get(:sonic_pi_spider_subthread_mutex).synchronize do
           start_subthreads = Thread.current.thread_variable_get(:sonic_pi_spider_subthreads).to_a
         end

         ## Set this thread's out bus to pipe audio into the new fx synth node
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_synth_out_bus, new_bus)

         ## Create a synth tracker and stick it in a thread local
         tracker = SynthTracker.new
         current_trackers = Thread.current.thread_variable_get(:sonic_pi_mod_sound_trackers) || Set.new
         current_trackers << tracker
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_trackers, current_trackers)

         ## Run fx block
         begin
           if block.arity == 0
             block.call
           else
             block.call(fx_synth)
           end
         rescue Exception => e
           # TODO: do something more sensible here
           puts "oopsey #{e}"
         end
         ## Remove synth tracker
         current_trackers.delete tracker

         ## Reset out bus to value prior to this with_fx block
         Thread.current.thread_variable_set(:sonic_pi_mod_sound_synth_out_bus, current_bus)

         ## Get a list of subthreads created by this fx block and create
         ## a new thread which will wait for them all to finish before
         ## killing the fx synth node and free its bus
         Thread.current.thread_variable_get(:sonic_pi_spider_subthread_mutex).synchronize do
           end_subthreads = Thread.current.thread_variable_get(:sonic_pi_spider_subthreads).to_a
         end
         new_subthreads = (end_subthreads - start_subthreads)
         Thread.new do
           new_subthreads.each do |st|
             __join_thread_and_subthreads(st)
           end

           tracker.block_until_finished

           Kernel.sleep(kill_delay)
           fx_synth.kill(true)
           new_bus.free
         end
       end

       def use_tempo(n, &block)
         raise "use_tempo does not work with a block. Perhaps you meant with_tempo" if block
         @mod_sound_studio.bpm = n
       end

       def with_tempo(n, &block)
         raise "with_tempo must be called with a block. Perhaps you meant use_tempo" unless block
         current = @mod_sound_studio.bpm
         @mod_sound_studio.bpm = n
         block.call
         @mod_sound_studio.bpm = current
       end

       def current_tempo
         @mod_sound_studio.bpm
       end

       def set_debug_on!
         @mod_sound_studio.debug = true
       end

       def set_debug_off!
         @mod_sound_studio.debug = false
       end

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
         samples_path + "/" + sym.to_s + ".wav"
       end

       def load_sample(path)
         if path.class == Symbol
           full_path = resolve_sample_symbol_path(path)
         end
         if File.exists?(full_path)
           @mod_sound_studio.load_sample(full_path)
         else
           if path.class == Symbol
             raise "No sample exists called #{path.inspect}"
           else
             raise "No sample exists with path #{path}"
           end
         end
       end

       def load_samples(*paths)
         paths.each{|p| load_sample p}
       end

       def sample_info(path)
         load_sample(path)
       end

       def sample_duration(path, *args)
         args_h = resolve_synth_opts_hash_or_array(args)
         args_h = {:rate => 1}.merge(args_h)
         load_sample(path).duration * 1.0/args_h[:rate]
       end

       def sample(path, *args)
         buf_info = load_sample(path)

         trigger_sampler path, buf_info.id, buf_info.num_chans, args
       end

       def status
         __message @mod_sound_studio.status
       end

       def note(n, o=nil)
         Note.resolve_midi_note(n, o)
       end

       def note_info(n, o=nil)
         Note.new(n, o)
       end

       def scale(tonic, name, *opts)
         opts = resolve_synth_opts_hash_or_array(opts)
         opts = {:num_octaves => 1}.merge(opts)
         Scale.new(tonic, name,  opts[:num_octaves]).to_a
       end

       def chord(tonic, name)
         Chord.new(tonic, name).to_a
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

       def trigger_sampler(path, buf_id, num_chans, args_a_or_h, group=current_job_synth_group)
         args_h = resolve_synth_opts_hash_or_array(args_a_or_h)
         args_h_with_buf = {:buf => buf_id}.merge(args_h)

         synth_name = (num_chans == 1) ? "mono_player" : "stereo_player"
         validation_fn = mk_synth_args_validator(synth_name)
         validation_fn.call(args_h_with_buf)

         if args_h.empty?
           __message "Playing sample: #{path}"
         else
           __message "Playing sample: #{path} with args: #{arg_h_pp(args_h)}"
         end

         trigger_synth(synth_name, args_h_with_buf, group, false, validation_fn)
       end

       def trigger_inst(synth_name, args_a_or_h, group=current_job_synth_group)
         args_h = resolve_synth_opts_hash_or_array(args_a_or_h)

         validation_fn = mk_synth_args_validator(synth_name)
         validation_fn.call(args_h)
         __message "Playing #{synth_name} with args: #{arg_h_pp(args_h)}"
         trigger_synth(synth_name, args_h, group, false, validation_fn)
       end

       def trigger_chord(synth_name, notes, args_a_or_h, group=current_job_synth_group)
         args_h = resolve_synth_opts_hash_or_array(args_a_or_h)

         validation_fn = mk_synth_args_validator(synth_name)
         validation_fn.call(args_h)

         chord_group = @mod_sound_studio.new_group(:tail, group)
         cg = ChordGroup.new(chord_group)

         nodes = []
         notes.each do |note|
           note_args_h = args_h.merge({:note => note})
           nodes << trigger_inst(synth_name, note_args_h, cg) if note
         end
         cg.sub_nodes = nodes
         cg
       end

       def trigger_fx(synth_name, args_a_or_h, group=current_fx_group)
         args_h = resolve_synth_opts_hash_or_array(args_a_or_h)
         validation_fn = mk_synth_args_validator(synth_name)
         validation_fn.call(args_h)
         trigger_synth(synth_name, args_h, group, true, validation_fn)
       end

       def trigger_synth(synth_name, args_h, group, now=false, arg_validation_fn)
         synth_name = "sp/#{synth_name}"

         current_bus = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_out_bus)
         out_bus = current_bus || @mod_sound_studio.mixer_bus

         job_id = current_job_id
         t_l_args = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults) || {}
         combined_args = t_l_args.merge(args_h).merge({"out-bus" => out_bus})
         p = Promise.new
         job_synth_proms_add(job_id, p)

         s = @mod_sound_studio.trigger_synth synth_name, group, combined_args, now, &arg_validation_fn


         trackers = (Thread.current.thread_variable_get(:sonic_pi_mod_sound_trackers) || []).to_a

         trackers.each{|t| t.synth_started(s)}
         s.on_destroyed do
           trackers.each{|t| t.synth_finished(s)}
           job_synth_proms_rm(job_id, p)
           p.deliver! true
         end
         s
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
     end
   end
 end
