#!/usr/bin/env ruby

#$: << File.expand_path('../../lib', __FILE__)

require 'benchmark'
require 'benchmark/ips'

require 'concurrent'
require 'celluloid'

class CelluloidClass
  include Celluloid
  def foo(latch = nil)
    latch.count_down if latch
  end
end

class AsyncClass
  include Concurrent::Async
  def foo(latch = nil)
    latch.count_down if latch
  end
end

IPS_NUM = 100
BMBM_NUM = 100_000
SMALL_BMBM = 250

puts "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
puts "Long-lived objects"
puts "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
puts ""

Benchmark.ips do |bm|
  celluloid = CelluloidClass.new
  bm.report('celluloid') do
    latch = Concurrent::CountDownLatch.new(IPS_NUM)
    IPS_NUM.times { celluloid.async.foo(latch) }
    latch.wait
  end

  async = AsyncClass.new
  bm.report('async') do
    latch = Concurrent::CountDownLatch.new(IPS_NUM)
    IPS_NUM.times { async.async.foo(latch) }
    latch.wait
  end

  bm.compare!
end

Benchmark.bmbm do |bm|
  celluloid = CelluloidClass.new
  bm.report('celluloid') do
    latch = Concurrent::CountDownLatch.new(BMBM_NUM)
    BMBM_NUM.times { celluloid.async.foo(latch) }
    latch.wait
  end

  async = AsyncClass.new
  bm.report('async') do
    latch = Concurrent::CountDownLatch.new(BMBM_NUM)
    BMBM_NUM.times { async.async.foo(latch) }
    latch.wait
  end
end

puts ""
puts "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
puts "Short-lived objects"
puts "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
puts ""

Benchmark.ips do |bm|
  bm.report('future') do
    latch = Concurrent::CountDownLatch.new(IPS_NUM)
    IPS_NUM.times do
      Concurrent::Future.execute { latch.count_down  }
    end
    latch.wait
  end

  async = AsyncClass.new
  bm.report('async') do
    latch = Concurrent::CountDownLatch.new(IPS_NUM)
    IPS_NUM.times { AsyncClass.new.async.foo(latch) }
    latch.wait
  end

  bm.compare!
end

Benchmark.bmbm do |bm|
  bm.report('celluloid') do
    latch = Concurrent::CountDownLatch.new(SMALL_BMBM)
    SMALL_BMBM.times { CelluloidClass.new.async.foo(latch) }
    latch.wait
  end

  bm.report('async') do
    latch = Concurrent::CountDownLatch.new(SMALL_BMBM)
    SMALL_BMBM.times { AsyncClass.new.async.foo(latch) }
    latch.wait
  end
end

__END__

===========================================================
  Async Benchmarks
===========================================================

  Computer:

  * OS X Yosemite
- Version 10.10.4
* MacBook Pro
- Retina, 13-inch, Early 2015
* Processor 3.1 GHz Intel Core i7
* Memory 16 GB 1867 MHz DDR3
* Physical Volumes:
  - Apple SSD SM0512G
- 500 GB

===========================================================
  ruby 2.2.2p95 (2015-04-13 revision 50295) [x86_64-darwin14]
===========================================================

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Long-lived objects
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Calculating -------------------------------------
           celluloid    22.000  i/100ms
               async    37.000  i/100ms
-------------------------------------------------
           celluloid    239.639  (±10.8%) i/s -      1.188k
               async    374.885  (± 2.7%) i/s -      1.887k

Comparison:
               async:      374.9 i/s
           celluloid:      239.6 i/s - 1.56x slower

Rehearsal ---------------------------------------------
celluloid   3.910000   0.540000   4.450000 (  4.455316)
async       2.730000   0.010000   2.740000 (  2.736720)
------------------------------------ total: 7.190000sec

                user     system      total        real
celluloid   3.880000   0.550000   4.430000 (  4.435163)
async       2.740000   0.010000   2.750000 (  2.750706)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Short-lived objects
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Calculating -------------------------------------
              future    19.000  i/100ms
               async    19.000  i/100ms
-------------------------------------------------
              future    191.738  (± 3.7%) i/s -    969.000
               async    188.085  (± 4.3%) i/s -    950.000

Comparison:
              future:      191.7 i/s
               async:      188.1 i/s - 1.02x slower

Rehearsal ---------------------------------------------
celluloid   0.110000   0.020000   0.130000 (  0.131996)
async       0.040000   0.010000   0.050000 (  0.037236)
------------------------------------ total: 0.180000sec

                user     system      total        real
celluloid   0.160000   0.040000   0.200000 (  0.186817)
async       0.040000   0.010000   0.050000 (  0.051579)

===========================================================
jruby 9.0.1.0 (2.2.2) 2015-09-02 583f336 Java HotSpot(TM) 64-Bit Server VM 25.45-b02 on 1.8.0_45-b14 +jit [darwin-x86_64]
===========================================================

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Long-lived objects
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Calculating -------------------------------------
           celluloid     1.000  i/100ms
               async    14.000  i/100ms
-------------------------------------------------
           celluloid    139.631  (±42.3%) i/s -    473.000
               async    883.424  (±26.6%) i/s -      3.514k

Comparison:
               async:      883.4 i/s
           celluloid:      139.6 i/s - 6.33x slower

Rehearsal ---------------------------------------------
celluloid   7.420000   1.930000   9.350000 (  6.625224)
async       2.630000   0.210000   2.840000 (  1.574823)
----------------------------------- total: 12.190000sec

                user     system      total        real
celluloid   5.910000   1.720000   7.630000 (  5.995677)
async       2.610000   0.190000   2.800000 (  1.594092)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Short-lived objects
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Calculating -------------------------------------
              future    40.000  i/100ms
               async    48.000  i/100ms
-------------------------------------------------
              future    640.057  (± 4.8%) i/s -      3.200k
               async    570.240  (± 4.7%) i/s -      2.880k

Comparison:
              future:      640.1 i/s
               async:      570.2 i/s - 1.12x slower

Rehearsal ---------------------------------------------
celluloid   1.420000   0.090000   1.510000 (  0.523106)
async       0.020000   0.000000   0.020000 (  0.006935)
------------------------------------ total: 1.530000sec

                user     system      total        real
celluloid   0.620000   0.100000   0.720000 (  0.293182)
async       0.020000   0.000000   0.020000 (  0.007434)
