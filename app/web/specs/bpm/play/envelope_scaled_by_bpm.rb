# envelope times are beats: at 120 bpm each is halved in seconds
use_bpm 120   # at twice the tempo: specs/play/envelope_scaled_by_bpm.rb
use_bpm 240
play 60, attack: 1, decay: 1, sustain: 2, release: 1
sleep 1
play 60, release: 0.5
