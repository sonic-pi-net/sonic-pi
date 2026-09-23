# in_thread(sync: ...) waits for the cue before its body runs
use_bpm 120   # at twice the tempo: specs/in_thread/sync_opt.rb
in_thread(sync: :start) do
  play 60
end
sleep 0.25
cue :start
