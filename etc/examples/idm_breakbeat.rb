def play_bb(n)
  sample :breakbeat, :attack, 0, :release, 0.05, :start, 1 - (1.0 / n)
  sleep sample_duration(:breakbeat) / n
end

loop {play_bb([1,2,4,8,16].choose)}
