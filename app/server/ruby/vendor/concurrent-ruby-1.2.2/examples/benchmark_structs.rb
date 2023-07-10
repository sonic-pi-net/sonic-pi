#!/usr/bin/env ruby

#$: << File.expand_path('../../lib', __FILE__)

require 'benchmark'
require 'concurrent'

n = 500_000

StructPair = Struct.new(:left, :right)
SafePair = Concurrent::MutableStruct.new(:left, :right)
FinalPair = Concurrent::SettableStruct.new(:left, :right)
ImmutablePair = Concurrent::ImmutableStruct.new(:left, :right)

array_pair = [true, false].freeze
struct_pair = StructPair.new(true, false)
safe_pair = SafePair.new(true, false)
final_pair = FinalPair.new(true, false)
immutable = ImmutablePair.new(true, false)

puts "Object creation...\n"
Benchmark.bmbm do |x|
  x.report('create frozen array') { n.times{ [true, false].freeze } }
  x.report('create frozen struct') { n.times{ StructPair.new(true, false).freeze } }
  x.report('create mutable struct') { n.times{ SafePair.new(true, false) } }
  x.report('create settable struct') { n.times{ FinalPair.new(true, false) } }
  x.report('create immutable struct') { n.times{ ImmutablePair.new(true, false) } }
end

puts "\n"

puts "Object access...\n"
Benchmark.bmbm do |x|
  x.report('read from frozen array') { n.times{ array_pair.last } }
  x.report('read from frozen struct') { n.times{ struct_pair.right } }
  x.report('read from mutable struct') { n.times{ safe_pair.right } }
  x.report('read from settable struct') { n.times{ final_pair.right } }
  x.report('read from immutable struct') { n.times{ immutable.right } }
end

puts "\n"

puts "Enumeration...\n"
Benchmark.bmbm do |x|
  x.report('iterate over frozen array') { n.times{ array_pair.each{ nil } } }
  x.report('iterate over frozen struct') { n.times{ struct_pair.each{ nil } } }
  x.report('iterate over mutable struct') { n.times{ safe_pair.each{ nil } } }
  x.report('iterate over settable struct') { n.times{ final_pair.each{ nil } } }
  x.report('iterate over immutable struct') { n.times{ immutable.each{ nil } } }
end

__END__

Object creation...
Rehearsal -----------------------------------------------------------
create frozen array       0.090000   0.000000   0.090000 (  0.091262)
create frozen struct      0.180000   0.000000   0.180000 (  0.179993)
create mutable struct     2.030000   0.000000   2.030000 (  2.052071)
create settable struct    2.070000   0.000000   2.070000 (  2.080022)
create immutable struct   0.710000   0.000000   0.710000 (  0.716877)
-------------------------------------------------- total: 5.080000sec

                              user     system      total        real
create frozen array       0.100000   0.000000   0.100000 (  0.097776)
create frozen struct      0.190000   0.000000   0.190000 (  0.186287)
create mutable struct     2.020000   0.010000   2.030000 (  2.032391)
create settable struct    2.030000   0.000000   2.030000 (  2.031631)
create immutable struct   0.690000   0.000000   0.690000 (  0.695010)

Object access...
Rehearsal --------------------------------------------------------------
read from frozen array       0.060000   0.000000   0.060000 (  0.060430)
read from frozen struct      0.060000   0.000000   0.060000 (  0.058978)
read from mutable struct     0.440000   0.000000   0.440000 (  0.454071)
read from settable struct    0.460000   0.000000   0.460000 (  0.457699)
read from immutable struct   0.120000   0.000000   0.120000 (  0.126701)
----------------------------------------------------- total: 1.140000sec

                                 user     system      total        real
read from frozen array       0.060000   0.000000   0.060000 (  0.063006)
read from frozen struct      0.060000   0.000000   0.060000 (  0.094203)
read from mutable struct     0.420000   0.000000   0.420000 (  0.468304)
read from settable struct    0.410000   0.000000   0.410000 (  0.452446)
read from immutable struct   0.110000   0.010000   0.120000 (  0.127030)

Enumeration...
Rehearsal -----------------------------------------------------------------
iterate over frozen array       0.170000   0.000000   0.170000 (  0.176898)
iterate over frozen struct      0.160000   0.000000   0.160000 (  0.160786)
iterate over mutable struct     1.520000   0.000000   1.520000 (  1.627013)
iterate over settable struct    1.500000   0.010000   1.510000 (  1.525163)
iterate over immutable struct   0.990000   0.000000   0.990000 (  1.006201)
-------------------------------------------------------- total: 4.350000sec

                                    user     system      total        real
iterate over frozen array       0.170000   0.000000   0.170000 (  0.167927)
iterate over frozen struct      0.150000   0.000000   0.150000 (  0.157328)
iterate over mutable struct     1.450000   0.000000   1.450000 (  1.462654)
iterate over settable struct    1.460000   0.000000   1.460000 (  1.480270)
iterate over immutable struct   0.940000   0.010000   0.950000 (  0.955633)
