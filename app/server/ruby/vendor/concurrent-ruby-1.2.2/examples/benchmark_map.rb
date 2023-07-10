#!/usr/bin/env ruby

require 'benchmark/ips'
require 'concurrent/map'

hash = {}
map = Concurrent::Map.new

ENTRIES = 10_000

ENTRIES.times do |i|
  hash[i] = i
  map[i] = i
end

TESTS = 1_000
key = 2732 # srand(0) and rand(10_000)

Benchmark.ips do |results|
  results.report('Hash#[]') do
    hash[key]
  end

  results.report('Map#[]') do
    map[key]
  end

  results.report('Hash#each_pair') do
    hash.each_pair { |k,v| v }
  end

  results.report('Map#each_pair') do
    map.each_pair { |k,v| v }
  end
end
