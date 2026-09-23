# sample defaults do not reach play, and synth defaults do not reach sample
use_bpm 120   # at twice the tempo: specs/sample/defaults_do_not_affect_play.rb
use_sample_defaults amp: 0.5
use_synth_defaults amp: 0.25
sample :loop_amen
play 60
