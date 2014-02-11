require "benchmark/ips"
require "hamster/hash"

Benchmark.ips do |b|
  sml_hash = Hamster.hash(1 => 1)
  med_hash = Hamster.hash
  1_000.times { |i| med_hash = med_hash.put(i, i) }
  lrg_hash = Hamster.hash
  1_000_000.times { |i| lrg_hash = lrg_hash.put(i, i) }

  b.report "put value" do |n|
    a = 0
    sml = sml_hash
    while a < n
      sml = sml.put(a, a)
      a += 1
    end
  end

  b.report "put value medium" do |n|
    a = 0
    med = med_hash
    while a < n
      med = med.put(a, a)
      a += 1
    end
  end

  b.report "put value large" do |n|
    a = 0
    lrg = lrg_hash
    while a < n
      lrg = lrg.put(a, a)
      a += 1
    end
  end
end
