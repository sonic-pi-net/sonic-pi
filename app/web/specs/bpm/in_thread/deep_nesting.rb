# five levels of nesting, each a quarter beat later
use_bpm 120   # at twice the tempo: specs/in_thread/deep_nesting.rb
def nest(d)
  in_thread do
    sleep 0.1
    play 60 + d
    nest(d + 1) if d < 4
  end
end
nest(0)
