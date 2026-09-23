# a map answers as native's does: select, reject, slice, dig, invert, key, compact, fetch_values, comparison
use_bpm 120   # at twice the tempo: specs/lang/map_methods.rb
m = (map a: 1, b: 2, c: nil)
puts m.select { |k, v| v == 1 }
puts m.reject { |k, v| v.nil? }
puts m.slice(:a, :c)
puts (map x: (map y: 3)).dig(:x, :y)
puts (map a: 1, b: 2).invert
puts m.key(2)
puts m.compact
puts m.fetch_values(:a, :b)
puts m.has_value?(2)
puts m.include?(:c)
puts (map a: 1) < (map a: 1, b: 2)
puts (map a: 1, b: 2) >= (map a: 1)
puts m.any? { |k, v| v == 2 }
