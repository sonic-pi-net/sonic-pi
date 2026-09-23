# random sleep lengths move logical time by exactly what was drawn
use_random_seed 4
3.times do
  play 60
  sleep rrand(0.1, 0.3)
end
