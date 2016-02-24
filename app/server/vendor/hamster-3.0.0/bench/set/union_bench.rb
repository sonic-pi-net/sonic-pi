require "benchmark/ips"
require "hamster/set"

Benchmark.ips do |b|
  small_set = Hamster::Set.new((1..10).to_a)
  large_set = Hamster::Set.new((1..1000).to_a)

  b.report "small.union(large)" do
    small_set.union(large_set)
  end

  b.report "large.union(small)" do
    large_set.union(small_set)
  end
end
