# time_warp shifts the block's time without sleeping for it
play 60
time_warp 0.5 do
  play 62
end
play 64
time_warp -0.25 do
  play 65
end
sleep 1
play 67
