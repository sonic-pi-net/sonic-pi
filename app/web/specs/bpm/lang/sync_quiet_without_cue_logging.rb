# with cue logging off, a sync says nothing either, as a cue does not
use_bpm 120   # at twice the tempo: specs/lang/sync_quiet_without_cue_logging.rb
use_cue_logging false
in_thread do
  sleep 0.5
  cue :t
end
sync :t
play 60
