# a thread started inside nested blocks of one thread keeps both
use_bpm 120   # at twice the tempo: specs/with_fx/threads_in_inner_fx.rb
with_fx :level, kill_delay: 0.25 do
  with_fx :echo, decay: 0.25 do
    in_thread do
      sleep 1
      play 60, release: 0.25
    end
  end
end
