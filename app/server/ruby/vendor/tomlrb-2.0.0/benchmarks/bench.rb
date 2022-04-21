#!/usr/bin/env ruby
require 'benchmark/ips'
require_relative '../lib/tomlrb'
begin
  require 'toml-rb'
rescue LoadError
  puts "Install toml-rb using 'gem install toml-rb' first."
end

data = File.read(File.join(__dir__, '../test/example-v0.4.0.toml'))

Benchmark.ips do |x|

  x.report("emancu/toml-rb") do
    TomlRB.parse(data)
  end

  x.report("fbernier/tomlrb") do
    Tomlrb.parse(data)
  end

  x.compare!
end
