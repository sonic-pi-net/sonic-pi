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

module SonicPi
   module Mods
     module Sound

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

       def with_synth(synth_name)
         @mod_sound_studio.current_synth_name = synth_name
       end

       def trigger_sp_synth(synth_name, *args)
         job_id = current_job_id
         args_h = Hash[*args]
         t_l_args = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults) || {}
         args = t_l_args.merge(args_h).to_a.flatten
         p = Promise.new
         job_synth_proms_add(job_id, p)
         __message "playing #{synth_name} with: #{args}"
         s = @mod_sound_studio.trigger_sp_synth synth_name, job_synth_group(current_job_id), *args
         s.on_destroyed do
           job_synth_proms_rm(job_id, p)
           p.deliver! true
         end
         s
       end

       def trigger_synth(synth_name, *args)
         job_id = current_job_id
         args_h = Hash[*args]
         t_l_args = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults) || {}
         args = t_l_args.merge(args_h).to_a.flatten
         p = Promise.new
         job_synth_proms_add(job_id, p)
         __message "playing #{synth_name} with: #{args}"
         s = @mod_sound_studio.trigger_synth synth_name, job_synth_group(current_job_id), *args
         s.on_destroyed do
           job_synth_proms_rm(job_id, p)
           p.deliver! true
         end
         s
       end

       def play(n, *args)
         n = note(n)
         trigger_sp_synth @mod_sound_studio.current_synth_name, "note", n, *args if n
       end

       def repeat(&block)
         while true
           block.call
         end
       end

       def with_synth_defaults(*args)
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_defaults, Hash[*args]
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
         notes.each{|note| play(note, *args)}
       end

       def play_pad(name, *args)
         if args.size == 1
           @mod_sound_studio.switch_to_pad(name, "note", args[0])
         else
           @mod_sound_studio.switch_to_pad(name, *args)
         end
       end

       def control_pad(*args)
         @mod_sound_studio.control_pad(*args)
       end

       def comms_eval(code)
         eval(code)
         STDOUT.flush
         STDOUT.flush
         Thread.list.map {|t| t.join 60}
       end

       def debug!
         @mod_sound_studio.debug = true
       end

       def debug_off!
         @mod_sound_studio.debug = false
       end

       def with_volume(vol)
         if (vol < 0)
           @mod_sound_studio.volume = 0
         elsif (vol > 20)
           @mod_sound_studio.volume = 20
         else
           @mod_sound_studio.volume = vol
         end
       end

       def resolve_sample_symbol_path(sym)
         samples_path + "/" + sym.to_s + ".wav"
       end

       def load_sample(path)
         if path.class == Symbol
           path = resolve_sample_symbol_path(path)
         end

         @mod_sound_studio.load_sample(path)
       end

       def load_samples(*paths)
         paths.each{|p| load_sample p}
       end

       def sample_info(path)
         load_sample(path)
       end

       def sample_duration(path)
         load_sample(path).duration
       end

       def sample(path, *args)
         buf_info = load_sample(path)
         synth_name = (buf_info.num_chans == 1) ? "sp/mono-player" : "sp/stereo-player"
         __message "Playing sample: #{path}"
         trigger_synth synth_name, "buf", buf_info.id, *args
       end

       def status
         __message @mod_sound_studio.status
       end


       def note_info(n, o=nil)
         @mod_sound_studio.note(n, o)
       end

       def note(n, o=nil)
         case n
         when String
           @mod_sound_studio.note(n, o).midi_note
         when NilClass
           nil
         when Integer
           n
         when Float
           n
         when Symbol
           note(n.to_s, o)
         end
       end

       private

       def current_job_id
         Thread.current.thread_variable_get :sonic_pi_spider_job_id
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
     end
   end
 end
