require "benchmark/ips"
require "hamster/list"

Benchmark.ips do |b|
  sml_list = Hamster.list(1)
  med_list = Hamster.list
  100.times { |i| med_list = med_list.cons(i) }
  lrg_list = Hamster.list
  10000.times { |i| lrg_list = lrg_list.cons(i) }

  b.report "cons small" do |n|
    a = 0
    sml = sml_list
    while a < n
      sml = sml.cons(a)
      a += 1
    end
  end

  b.report "cons medium" do |n|
    a = 0
    med = med_list
    while a < n
      med = med.cons(a)
      a += 1
    end
  end

  b.report "cons large" do |n|
    a = 0
    lrg = lrg_list
    while a < n
      lrg = lrg.cons(a)
      a += 1
    end
  end
end
