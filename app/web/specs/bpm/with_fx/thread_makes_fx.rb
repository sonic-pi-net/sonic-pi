# a thread is done only when the fx it made has been freed
use_bpm 120   # at twice the tempo: specs/with_fx/thread_makes_fx.rb
with_fx :level, kill_delay: 0.25 do
  in_thread do
    with_fx :echo, decay: 0.5 do
      play 60, release: 0.25
    end
  end
end
