# use_cue_logging and with_cue_logging hide cue and set from the log; the values are still there
use_bpm 120   # at twice the tempo: specs/lang/cue_logging.rb
cue :a
use_cue_logging false
cue :b
set :x, 1
with_cue_logging true do
  cue :c
end
use_cue_logging true
set :y, 2
puts get(:x), get(:y)
