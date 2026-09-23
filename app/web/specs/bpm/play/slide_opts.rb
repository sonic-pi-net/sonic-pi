# per-opt slides pass through; slide: sets every slide at once
use_bpm 120   # at twice the tempo: specs/play/slide_opts.rb
play 60, amp_slide: 0.5
play 60, note_slide: 1, note_slide_shape: 5, note_slide_curve: 2
play 60, slide: 0.25, amp: 0.5, pan: 0.1
