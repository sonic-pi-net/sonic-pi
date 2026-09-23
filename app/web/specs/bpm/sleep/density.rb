# density n runs its block n times, each at n times the tempo
use_bpm 120   # at twice the tempo: specs/sleep/density.rb
density 2 do
  play 60
  sleep 1
end
play 62
