# an fx inside a thread wraps only that thread's synths
use_bpm 120   # at twice the tempo: specs/in_thread/with_fx_inside_thread.rb
in_thread do
  with_fx :reverb do
    play 60
  end
end
play 62
