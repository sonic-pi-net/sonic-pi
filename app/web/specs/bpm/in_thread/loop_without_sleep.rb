# a loop that never sleeps or syncs is stopped with an error
use_bpm 120   # at twice the tempo: specs/in_thread/loop_without_sleep.rb
in_thread do
  loop do
    play 60
  end
end
