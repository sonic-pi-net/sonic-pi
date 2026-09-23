# cue's map form takes immutable values only: a string that is not frozen is refused, and says why
use_bpm 120   # at twice the tempo: specs/lang/cue_map_needs_immutable.rb
cue :x, n: "c"
