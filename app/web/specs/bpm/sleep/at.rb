# at schedules blocks in their own threads at the given offsets
use_bpm 120   # at twice the tempo: specs/sleep/at.rb
at [0, 0.5, 1] do |t|
  play 60 + t * 2
end
at 0.25, [:a, :b] do |t, v|
  puts v
end
play 72
