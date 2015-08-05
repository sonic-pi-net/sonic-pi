#!/usr/bin/env ruby

$: << File.expand_path('../../lib', __FILE__)

require 'benchmark'
require 'concurrent/executors'

COUNT = 1000

if Concurrent.on_jruby?
  EXECUTORS = [
    [Concurrent::JavaCachedThreadPool],
    [Concurrent::JavaFixedThreadPool, 10],
    [Concurrent::JavaSingleThreadExecutor],
    [Concurrent::JavaThreadPoolExecutor]
  ]
else
  EXECUTORS = [
    [Concurrent::CachedThreadPool],
    [Concurrent::FixedThreadPool, 10],
    [Concurrent::SingleThreadExecutor],
    [Concurrent::ThreadPoolExecutor]
  ]
end

Benchmark.bmbm do |x|
  EXECUTORS.each do |executor_class, *args|
    x.report(executor_class.to_s) do
      if args.empty?
        executor = executor_class.new
      else
        executor = executor_class.new(*args)
      end
      COUNT.times { executor.post{ nil } }
    end
  end
end
