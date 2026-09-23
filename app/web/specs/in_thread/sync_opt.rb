# in_thread(sync: ...) waits for the cue before its body runs
in_thread(sync: :start) do
  play 60
end
sleep 0.25
cue :start
