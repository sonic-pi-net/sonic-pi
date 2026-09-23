# with_swing, tuplets, with_sched_ahead_time, with_real_time, wait, on, comment and uncomment
8.times do
  with_swing 0.1, pulse: 4 do
    play 60, release: 0.1
  end
  sleep 0.25
end
tuplets [[60, 62, 64], 65], duration: 0.5 do |n|
  play n, release: 0.1
end
with_sched_ahead_time 1 do
  play 70
end
with_real_time do
  play 72
end
wait 0.5
on true do
  play 74
end
on 0 do
  play 75
end
comment do
  play 76
end
uncomment do
  play 77
end
