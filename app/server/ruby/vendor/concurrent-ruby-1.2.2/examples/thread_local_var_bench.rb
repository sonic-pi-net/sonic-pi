#!/usr/bin/env ruby

#$: << File.expand_path('../../lib', __FILE__)

require 'concurrent'
require 'concurrent/atomic/thread_local_var'
require 'benchmark'

include Concurrent

N_THREADS = 100
N_VARS    = 100

vars = N_VARS.times.collect { ThreadLocalVar.new(0) }

def test_threadlocal_perf(vars)
  threads = N_THREADS.times.collect do
    Thread.new do
      10000.times do
        index = rand(N_VARS)
        var   = vars[index]
        var.value = var.value + 1
      end
    end
  end
  threads.each(&:join)
end

Benchmark.bmbm do |bm|
  bm.report('ThreadLocalVar') { test_threadlocal_perf(vars) }
end
