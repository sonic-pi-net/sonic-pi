# opts can come from a hash held in a variable, and merged
use_bpm 120   # at twice the tempo: specs/play/opts_hash_variable.rb
o = { amp: 0.7, pan: -0.5 }
play 60, o
play 60, o.merge(release: 0.1)
play 60, **o, amp: 0.2
