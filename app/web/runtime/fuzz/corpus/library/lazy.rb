# enumerators, lazy chains, fibers and ranges
e = [1, 2, 3].each
puts e.next, e.next, e.next
begin; e.next; rescue StopIteration; puts "done"; end
puts (1..Float::INFINITY).lazy.map { |x| x * 2 }.select { |x| x % 3 == 0 }.first(4).inspect
puts (1..20).each_slice(6).map(&:sum).inspect, (1..10).step(3).to_a.inspect, 1.step(10, 4).to_a.inspect, (1.0..2.0).step(0.5).to_a.inspect
puts ("a".."e").to_a.join, (1...10).size, (1..10).include?(5.5), (1..).first(3).inspect, (1..10) === 3, (1..10).minmax.inspect
g = Enumerator.new { |y| y << 1; y << 2; y.yield 3 }
puts g.to_a.inspect, g.with_index(1).to_a.inspect, g.zip(g).size, [4, 5].each_with_index.to_a.inspect, g.chain([9]).to_a.size
f = Fiber.new { |x| y = Fiber.yield(x + 1); Fiber.yield(y * 2); :end }
puts f.resume(1), f.resume(10), f.resume, f.alive?
puts catch(:out) { 10.times { |i| throw :out, i if i == 3 }; :never }
play 60 + g.first
