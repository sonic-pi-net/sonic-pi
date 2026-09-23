# live_loop sync: waits for the cue before its first pass
live_loop :leader do
  sleep 0.25
  cue :go
  stop
end
live_loop :follower, sync: :go do
  play 60
  stop
end
