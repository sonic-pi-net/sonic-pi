# use_* inside a thread stays inside it
use_bpm 120   # at twice the tempo: specs/in_thread/settings_do_not_leak_out.rb
in_thread do
  use_synth :saw
  use_bpm 240
  use_transpose 12
  use_synth_defaults amp: 0.1
  play 60
end
sleep 0.5
play 60
