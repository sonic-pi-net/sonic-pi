require "benchmark/ips"
require "hamster/list"

Benchmark.ips do |b|
  sml_list = Hamster.list(1)
  # med_list = Hamster.iterate(1, &:next).take(100)
  # lrg_list = Hamster.iterate(1, &:next).take(10000)
  med_list = Hamster.list
  100.times { |i| med_list = med_list.cons(i) }
  lrg_list = Hamster.list
  10000.times { |i| lrg_list = lrg_list.cons(i) }

  b.report "at small" do |n|
    a = 0
    x = 0
    while a < n
      x = sml_list.at(0)
      a += 1
    end
  end

  b.report "at medium" do |n|
    a = 0
    x = 0
    while a < n
      x = med_list.at(99)
      a += 1
    end
  end

  b.report "at large" do |n|
    a = 0
    x = 0
    while a < n
      x = lrg_list.at(9999)
      a += 1
    end
  end
end
