# delay: postpones the thread's start
use_bpm 120   # at twice the tempo: specs/in_thread/delay_opt.rb
in_thread(delay: 0.5) do
  play 60
end
play 62
