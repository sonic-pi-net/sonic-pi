# the array and enumerable methods beyond the core
a = [5, 3, 8, 1, 9, 2, 7]
puts a.sort.bsearch { |x| x >= 4 }, a.rotate(2).inspect, a.each_slice(3).to_a.inspect, a.each_cons(2).count, a.combination(2).count
puts a.permutation(2).first(2).inspect, a.product([1, 2]).size, a.zip(a.reverse).flatten.uniq.size, a.partition(&:even?).inspect
puts a.minmax.inspect, a.sum, a.inject(:*), a.take_while { |x| x > 2 }.inspect, a.drop_while { |x| x > 2 }.size, a.chunk_while { |x, y| y > x }.to_a.inspect
puts a.group_by { |x| x % 3 }.inspect, a.each_with_index.map { |x, i| x * i }.inspect, a.each_with_object([]) { |x, m| m.unshift(x) }.inspect
puts a.tally.size, a.fill(0, 5).inspect, a.sample.class, a.shuffle.size, a.flat_map { |x| [x, x] }.size, a.cycle.take(9).size, a.dig(0)
puts a.values_at(0, -1, 99).inspect, a.assoc(1).inspect, a.transpose.inspect rescue puts "odd shape"
h = { a: 1, b: 2, c: 3 }
puts h.select { |_, v| v > 1 }.keys.inspect, h.transform_values { |v| v * 2 }.inspect, h.invert.inspect, h.min_by { |_, v| -v }.inspect, h.to_a.flatten.size
puts h.merge(d: 4) { |_, x, y| x + y }.size, h.filter_map { |k, v| k if v.odd? }.inspect, h.sum { |_, v| v }, h.any? { |_, v| v > 2 }, h.dig(:a), h.fetch(:z, 0)
play a.size + 60
