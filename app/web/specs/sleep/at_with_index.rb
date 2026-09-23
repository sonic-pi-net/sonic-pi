# at blocks receive time, value and index
at [0.1, 0.2], [60, 62] do |t, n, i|
  play n + i
end
