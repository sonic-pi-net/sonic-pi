# Set: the algebra of sets
a = Set.new([1, 2, 3, 4])
b = Set[3, 4, 5]
puts (a | b).size, (a & b).to_a.inspect, (a - b).to_a.inspect, (a ^ b).to_a.sort.inspect
puts a.subset?(b), a.superset?(Set[1]), a.include?(2), a.disjoint?(Set[9])
a << 10; a.delete(1); a.merge([7, 8])
puts a.map { |x| x * 2 }.sort.inspect, a.select(&:even?).size, a.group_by(&:odd?).keys.inspect
c = a.dup; c.clear; puts c.empty?, a == a.dup, a.hash == a.dup.hash
play a.min + 50
