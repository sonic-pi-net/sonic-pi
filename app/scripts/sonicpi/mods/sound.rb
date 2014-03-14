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
require_relative "../note"
require_relative "../scale"
require_relative "../chord"
require_relative "../chordgroup"

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
             @JOB_SYNTH_PROMS_A = Atom.new(Hamster.hash)
             @JOB_GROUPS_A = Atom.new(Hamster.hash)
             @JOB_GROUP_MUTEX = Mutex.new
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

       def with_synth(synth_name, &block)
         orig_synth = @mod_sound_studio.current_synth_name
         if block
           @mod_sound_studio.current_synth_name = synth_name
           block.call
           @mod_sound_studio.current_synth_name = orig_synth
         else
           @mod_sound_studio.current_synth_name = synth_name
         end
       end

       def trigger_synth(synth_name, args_h, group=current_job_synth_group)
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

         arg_validation_fn.call(args_h)

         synth_name = "sp/#{synth_name}"

         job_id = current_job_id
         t_l_args = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults) || {}
         combined_args = t_l_args.merge(args_h)
         flattened_args = combined_args.to_a.flatten
         p = Promise.new
         job_synth_proms_add(job_id, p)
         __message "playing #{synth_name} with: #{flattened_args}"
         s = @mod_sound_studio.trigger_synth synth_name, group, *flattened_args, &arg_validation_fn
         s.on_destroyed do |sn|
           job_synth_proms_rm(job_id, p)
           p.deliver! true
         end
         s
       end

       def play(n, *args)
         return play_chord(n, *args) if n.is_a?(Array)
         n = note(n)
         args_h = resolve_synth_opts_hash_or_array(args)
         args_h = {:note => n}.merge(args_h)
         trigger_synth @mod_sound_studio.current_synth_name, args_h if n
       end

       def repeat(&block)
         while true
           block.call
         end
       end

       def with_merged_synth_defaults(*args, &block)
         current = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults)
         args_h = resolve_synth_opts_hash_or_array(args)
         merged = (current || {}).merge(args_h)
         if block
           Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, merged
           block.call
           Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, current
         else
           Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, merged
         end
       end

       def with_synth_defaults(*args, &block)
         current = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults)
         new = resolve_synth_opts_hash_or_array(args)
         if block
           Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, new
           block.call
           Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, current
         else
           Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, new
         end
       end

       def with_tempo(n)
         @mod_sound_studio.bpm = n
       end

       def current_tempo
         @mod_sound_studio.bpm
       end

       def play_pattern(notes, *args)
         notes.each{|note| play(note, *args) ; sleep(@mod_sound_studio.beat_s)}
       end

       def play_pattern_timed(notes, times, *args)
         notes.each_with_index{|note, idx| play(note, *args) ; sleep(times[idx % times.size])}
       end

       def play_chord(notes, *args)
         args_h = resolve_synth_opts_hash_or_array(args)
         g = @mod_sound_studio.new_group(:tail, current_job_synth_group)
         cg = ChordGroup.new(g)
         nodes = []
         notes.each do |note|
           note_args_h = {:note => note}.merge(args_h)
           nodes << trigger_synth(@mod_sound_studio.current_synth_name, note_args_h, cg) if note
         end
         cg.sub_nodes = nodes
         cg
       end

       def debug!
         @mod_sound_studio.debug = true
       end

       def debug_off!
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
         args_h = resolve_synth_opts_hash_or_array(args)
         args_h = {:buf => buf_info.id}.merge(args_h)
         synth_name = (buf_info.num_chans == 1) ? "mono_player" : "stereo_player"
         __message "Playing sample: #{path}"
         trigger_synth synth_name, args_h
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

       def current_job_id
         Thread.current.thread_variable_get :sonic_pi_spider_job_id
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
           g = @mod_sound_studio.new_user_synth_group

           @JOB_GROUPS_A.swap! do |gs|
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
         job_group.kill if job_group

       end

       def munge_synth_args(args)
         args = Hash[*args] if args.is_a? Array
         # ensure note is a MIDI note
         n = args[:note]
         args[:note] = note(n) if n

         args
       end

     end
   end
 end
