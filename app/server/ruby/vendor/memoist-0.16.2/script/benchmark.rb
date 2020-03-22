$LOAD_PATH.unshift File.expand_path(File.dirname(__FILE__) + '/../lib')
require 'benchmark/ips'

require 'memoist'

class Benchy
  extend Memoist

  def arity_0
    'Hello World'
  end
  memoize :arity_0

  def arity_1(name)
    "Hello #{name}"
  end
  memoize :arity_1
end

OBJECT = Benchy.new

puts "Benchmarking: #{Memoist::VERSION}"

Benchmark.ips do |x|
  x.report('arity 0 - memoized') do |times|
    times.times do
      OBJECT.arity_0
    end
  end

  # x.report("arity 0 - unmemoized") do |times|
  #   times.times do
  #     OBJECT._unmemoized_arity_0
  #   end
  # end

  x.report('arity 1 - memoized') do |times|
    times.times do
      OBJECT.arity_1(:World)
    end
  end

  # x.report("arity 1 - unmemoized") do |times|
  #   times.times do
  #     OBJECT._unmemoized_arity_1(:World)
  #   end
  # end
end
