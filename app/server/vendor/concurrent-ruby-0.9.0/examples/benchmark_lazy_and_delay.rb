#!/usr/bin/env ruby

$: << File.expand_path('../../lib', __FILE__)

require 'benchmark'
require 'thread'
require 'concurrent'

n = 500_000

class LazyReference
  def initialize(&block)
    raise ArgumentError.new('no block given') unless block_given?
    @task = block
    @mutex = Mutex.new
    @value = nil
    @fulfilled = false
  end

  def value
    @mutex.synchronize do
      unless @fulfilled
        begin
          @value = @task.call
        ensure
          @fulfilled = true
        end
      end
    end
    return @value
  end
end

delay = Concurrent::Delay.new{ nil }
lazy = LazyReference.new{ nil }

delay.value
lazy.value

Benchmark.bmbm do |x|
  x.report('Delay#value') { n.times{ delay.value } }
  x.report('Delay#value!') { n.times{ delay.value! } }
  x.report('LazyReference#value') { n.times{ lazy.value } }
end
