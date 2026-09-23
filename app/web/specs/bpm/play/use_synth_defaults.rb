# use_synth_defaults adds opts to every later play; explicit opts win
use_bpm 120   # at twice the tempo: specs/play/use_synth_defaults.rb
use_synth_defaults amp: 0.5, release: 2
play 60
play 60, amp: 1
use_synth_defaults cutoff: 80
play 60
