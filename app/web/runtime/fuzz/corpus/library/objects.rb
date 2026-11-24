# structs, methods as objects, eval and what a program can ask about itself
pt = Struct.new(:x, :y)
p1 = pt.new(3, 4)
puts p1.to_a.inspect, p1 == pt.new(3, 4), p1.members.inspect, pt.members.size, p1[0], p1.to_h.inspect
puts p1.each.to_a.size rescue puts "no"
m = 5.method(:+)
puts m.call(3), m.arity, m.owner
puts m.unbind.bind(7).call(1)
puts [1, 2].map(&10.method(:*)).inspect, m.to_proc.call(1)
add = proc { |a, b = 2, *c, k: 1| a + b + k }
puts add.call(1), add.arity, add.lambda?, add.parameters.inspect
puts add.curry.class
puts (add >> proc { |x| x * 2 }).call(1)
puts eval("1 + 2")
puts binding.local_variables.size > 0, binding.local_variable_get(:p1).x
puts instance_eval { self.class }, Object.new.instance_variable_set(:@a, 1)
puts p1.respond_to?(:x), pt.instance_methods(false).inspect, pt.ancestors.size > 1, defined?(zzz).inspect, p1.frozen?
puts ObjectSpace.count_objects.size > 0
puts GC.start.inspect, 1.then { |x| x + 1 }, 5.clamp(1, 3), 5.between?(1, 9), Comparable.instance_methods.size > 0
puts Random.new(1).rand(10).class, Random.rand.class
puts Math.hypot(3, 4), Math.log(8, 2), Math.cbrt(27), Math.atan2(1, 1).round(3)
play 60 + p1.x
