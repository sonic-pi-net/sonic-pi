# a time_warp moves logical time only: the synth is later, the wall clock is not
with_fx :echo, decay: 0.25 do
  time_warp 1 do
    play 60, release: 0.25
  end
end
