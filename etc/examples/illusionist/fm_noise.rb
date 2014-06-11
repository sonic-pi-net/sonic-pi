use_synth :fm

loop do
  p = play chord(:Eb3, :minor).choose - [0, 12, -12].choose, divisor: 0.01, div_slide: rrand(0, 100), depth: rrand(0.001, 2), attack: 0.01, release: rrand(0, 5), amp: 0.5
  control p, divisor: rrand(0.001, 50)
  sleep [0.5, 1, 2].choose
end
