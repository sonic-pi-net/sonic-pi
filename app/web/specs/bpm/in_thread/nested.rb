# threads can spawn threads; each inherits its parent's time
use_bpm 120   # at twice the tempo: specs/in_thread/nested.rb
in_thread do
  sleep 0.25
  in_thread do
    sleep 0.25
    play 60
  end
  play 62
end
play 64
