# at blocks receive time, value and index
use_bpm 120   # at twice the tempo: specs/sleep/at_with_index.rb
at [0.1, 0.2], [60, 62] do |t, n, i|
  play n + i
end
