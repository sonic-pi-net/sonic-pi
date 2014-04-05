define :play_bb do |n|
  sample :drum_heavy_kick
  sample :ambi_drone, rate: [0.25, 0.5, 0.125, 1].choose, amp: 0.25 if rand < 0.125
  sample :ambi_lunar_land, rate: [0.5, 0.125, 1, -1, -0.5].choose, amp: 0.25 if rand < 0.125
  sample :loop_amen, attack: 0, release: 0.05, start: 1 - (1.0 / n), rate: [1,1,1,1,1,1,-1].choose
  sleep sample_duration(:loop_amen) / n
end


loop {play_bb([1,2,4,8,16].choose)}
