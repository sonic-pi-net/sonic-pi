# play_pattern_timed plays notes with the given sleeps (cycled)
use_bpm 120   # at twice the tempo: specs/play/play_pattern_timed.rb
play_pattern_timed [60, 62, 64, 65], [0.25, 0.5]
sleep 0.25
play_pattern_timed [67, 69], 0.125
