# envelope opts route through the full player, in beats
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
use_bpm 120   # at twice the tempo: specs/sample/envelope.rb
load_sample :loop_amen
sample :loop_amen, attack: 0.1, release: 0.5
sample :loop_amen, attack: 0.1, decay: 0.2, sustain: 0.3, release: 0.4
sample :loop_amen, attack_level: 0.5, sustain_level: 0.5, decay_level: 0.5
