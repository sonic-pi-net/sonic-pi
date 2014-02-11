require "benchmark/ips"
require "hamster/hash"

Benchmark.ips do |b|
  sml_hash = Hamster.hash(1 => 1)
  med_hash = Hamster.hash
  1_000.times { |i| med_hash = med_hash.put(i, i) }
  lrg_hash = Hamster.hash
  1_000_000.times { |i| lrg_hash = lrg_hash.put(i, i) }

  b.report "each small" do |n|
    a = 0
    x = 0
    while a < n
      sml_hash.each { |y| x = y }
      a += 1
    end
  end

  b.report "each medium" do |n|
    a = 0
    x = 0
    while a < n
      med_hash.each { |y| x = y }
      a += 1
    end
  end

  b.report "each large" do |n|
    a = 0
    x = 0
    while a < n
      lrg_hash.each { |y| x = y }
      a += 1
    end
  end
end
