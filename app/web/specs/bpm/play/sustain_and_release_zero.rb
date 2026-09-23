# zero-length envelope stages pass through as zeros
use_bpm 120   # at twice the tempo: specs/play/sustain_and_release_zero.rb
play 60, attack: 0, sustain: 0, release: 0
