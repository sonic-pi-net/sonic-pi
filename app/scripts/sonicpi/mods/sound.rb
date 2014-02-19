 module SonicPi
   module Mods
     module Sound

       def self.included(base)
         base.instance_exec {alias_method :sonic_pi_mods_sound_initialize_old, :initialize}

         base.instance_exec do
           define_method(:initialize) do |*splat, &block|
             sonic_pi_mods_sound_initialize_old *splat, &block
             hostname, port, msg_queue, max_concurrent_synths = *splat
             @job_groups = {}
             @mod_sound_studio = Studio.new(hostname, port, msg_queue, max_concurrent_synths)
             @events.add_handler("/job-completed", @events.gensym("/mods-sound-job-completed")) do |payload|

               current_synth_proms.each{|csp| csp.get}
               current_synth_group.kill
             end


             @events.add_handler("/stop-job", @events.gensym("/mods-sound-stop-job")) do |payload|
               job_id = payload[:id]
               group = @job_groups[job_id]
               group.kill if group
               @job_groups.delete(job_id)
             end
           end
         end
       end

       def with_synth(synth_name)
         @mod_sound_studio.current_synth_name = synth_name
       end

       def trigger_sp_synth(synth_name, *args)
         args_h = Hash[*args]
         t_l_args = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults) || {}
         args = t_l_args.merge(args_h).to_a.flatten
         p = Promise.new
         current_synth_proms_add p
         __message "playing #{synth_name} with: #{args}"
         s = @mod_sound_studio.trigger_sp_synth synth_name, current_synth_group, *args
         s.on_destroyed{ p.deliver! true }
         s
       end

       def trigger_synth(synth_name, *args)
         args_h = Hash[*args]
         t_l_args = Thread.current.thread_variable_get(:sonic_pi_mod_sound_synth_defaults) || {}
         args = t_l_args.merge(args_h).to_a.flatten
         p = Promise.new
         current_synth_proms_add p
         __message "playing #{synth_name} with: #{args}"
         s = @mod_sound_studio.trigger_synth synth_name, current_synth_group, *args
         s.on_destroyed{ p.deliver! true }
         s
       end

       def play(note, *args)
         trigger_sp_synth @mod_sound_studio.current_synth_name, "note", note, *args
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

       def stop
         __message "Stopping..."
         @mod_sound_studio.stop
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

       def in_thread(&block)
         cur_t = Thread.current
         t = Thread.new do
           cur_t.thread_variables.each do |v|
             __message "-->> #{v} : #{cur_t.thread_variable_get(v)}"
             Thread.current.thread_variable_set(v, cur_t.thread_variable_get(v))
           end
           with_synth "pretty_bell"
           block.call
         end
         register_thread t
         t
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

       def sample_partial(path, *args)
         buf_info = load_sample(path)
         synth_name = (buf_info.num_chans == 1) ? "sp/mono-partial-playr" : "sp/stereo-partial-playr"
         __message "Playing sample: #{path}"
         trigger_synth synth_name, "buf", buf_info.id, *args
       end

       def status
         __message @mod_sound_studio.status
       end

       private

       def register_thread(t)
         ts = @sub_threads[current_job_id] || []
         @sub_threads[current_job_id] = ts << t
       end

       def current_job_id
         Thread.current.thread_variable_get :sonic_pi_spider_job_id
       end

       def current_synth_group
         g = Thread.current.thread_variable_get :sonic_pi_mod_sound_synth_group
         return g if g
         g = @mod_sound_studio.new_user_synth_group
         @job_groups[current_job_id] = g
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_group, g
         g
       end

       def current_synth_proms
         s = Thread.current.thread_variable_get :sonic_pi_mod_sound_synth_proms
         return s if s
         s = []
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_proms, s
         s
       end

       def current_synth_proms_add(p)
         proms = current_synth_proms
         Thread.current.thread_variable_set :sonic_pi_mod_sound_synth_proms, proms + [p]
       end
     end
   end
 end
