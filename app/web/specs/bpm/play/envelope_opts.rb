# the envelope opts are passed through in beats (60 bpm: seconds)
use_bpm 120   # at twice the tempo: specs/play/envelope_opts.rb
play 60, attack: 0.1, decay: 0.2, sustain: 0.3, release: 0.4
play 60, attack_level: 0.8, decay_level: 0.6, sustain_level: 0.4
play 60, env_curve: 3
