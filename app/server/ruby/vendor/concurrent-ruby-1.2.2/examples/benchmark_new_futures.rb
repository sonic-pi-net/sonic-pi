#!/usr/bin/env ruby

require 'benchmark/ips'
require 'concurrent'
require 'concurrent-edge'

raise 'concurrent-ext not loaded' if Concurrent.on_cruby? && Concurrent.c_extensions_loaded?

scale  = 1
time   = 10 * scale
warmup = 2 * scale
warmup *= 10 if Concurrent.on_jruby?

Benchmark.ips(time, warmup) do |x|
  x.report('flat-old') do
    Concurrent::Promise.execute { 1 }.flat_map { |v| Concurrent::Promise.execute { v + 2 } }.value!
  end
  x.report('flat-new') do
    Concurrent::Promises.future(:fast) { 1 }.then { |v| Concurrent::Promises.future(:fast) { v + 2 } }.flat.value!
  end
  x.compare!
end

Benchmark.ips(time, warmup) do |x|
  x.report('status-old') { f = Concurrent::Promise.execute { nil }; 100.times { f.complete? } }
  x.report('status-new') { f = Concurrent::Promises.future(:fast) { nil }; 100.times { f.resolved? } }
  x.compare!
end

Benchmark.ips(time, warmup) do |x|
  of = Concurrent::Promise.execute { 1 }
  nf = Concurrent::Promises.fulfilled_future(1, :fast)
  x.report('value-old') { of.value! }
  x.report('value-new') { nf.value! }
  x.compare!
end

Benchmark.ips(time, warmup) do |x|
  x.report('graph-old') do
    head = Concurrent::Promise.fulfill(1)
    10.times do
      branch1 = head.then(&:succ)
      branch2 = head.then(&:succ).then(&:succ)
      head    = Concurrent::Promise.zip(branch1, branch2).then { |a, b| a + b }
    end
    head.value!
  end
  x.report('graph-new') do
    head = Concurrent::Promises.fulfilled_future(1, :fast)
    10.times do
      branch1 = head.then(&:succ)
      branch2 = head.then(&:succ).then(&:succ)
      head    = (branch1 & branch2).then { |a, b| a + b }
    end
    head.value!
  end
  x.compare!
end

Benchmark.ips(time, warmup) do |x|
  x.report('immediate-old') { Concurrent::Promise.fulfill(nil).value! }
  x.report('immediate-new') { Concurrent::Promises.fulfilled_future(nil, :fast).value! }
  x.compare!
end

Benchmark.ips(time, warmup) do |x|
  of = Concurrent::Promise.execute { 1 }
  nf = Concurrent::Promises.fulfilled_future(1, :fast)
  x.report('then-old') { 50.times.reduce(of) { |nf, _| nf.then(&:succ) }.value! }
  x.report('then-new') { 50.times.reduce(nf) { |nf, _| nf.then(&:succ) }.value! }
  x.compare!
end
