 module SonicPi
   module Mods
     module Sound

       def self.included(base)
         base.instance_exec {alias_method :sonic_pi_mods_sound_initialize_old, :initialize}

         base.instance_exec do
           define_method(:initialize) do |*splat, &block|
             sonic_pi_mods_sound_initialize_old *splat, &block
             hostname, port, msg_queue, max_concurrent_synths = *splat
             @mod_sound_studio = Studio.new(hostname, port, msg_queue, max_concurrent_synths)
           end
         end
       end


       def with_synth(synth_name)
         @mod_sound_studio.current_synth_name = synth_name
       end

       def play_synth(synth_name, *args)
         message "playing #{synth_name} with: #{args}"
         STDOUT.flush
         STDOUT.flush
         @mod_sound_studio.trigger_synth synth_name, *args
       end

       def play(note, *args)
         play_synth @mod_sound_studio.current_synth_name, "note", note, *args
       end

       def repeat(&block)
         while true
           block.call
         end
       end

       def with_tempo(n)
         @mod_sound_studio.bpm = n
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
         message "Stopping..."
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
         Thread.new do
           with_synth "pretty_bell"
           block.call
         end
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

       def sample(path, *args)
         buf_info = load_sample(path)
         synth_name = (buf_info[:num_chans] == 1) ? "overtone.sc.sample/mono-player" : "overtone.sc.saddd/stereo-player"
         message "Playing sample: #{path}"
         @mod_sound_studio.trigger_non_sp_synth(synth_name, "buf", buf_info[:id], *args)
       end

       def status
         message @mod_sound_studio.status
       end

     end
   end
 end
