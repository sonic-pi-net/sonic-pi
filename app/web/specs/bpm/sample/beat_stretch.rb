# beat_stretch sets the rate so the sample lasts that many beats
use_bpm 120   # at twice the tempo: specs/sample/beat_stretch.rb
sample :loop_amen, beat_stretch: 2
sample :loop_amen, beat_stretch: 1
use_bpm 240
sample :loop_amen, beat_stretch: 4
