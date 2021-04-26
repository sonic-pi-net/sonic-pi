########################################################################
# bench_ps.rb
#
# Benchmark program to show overall speed and compare the block form
# versus the non-block form. You should run this benchmark via the
# 'rake bench' Rake task.
########################################################################
require 'benchmark'
require 'sys/proctable'

MAX = 10

Benchmark.bm do |bench|
   bench.report("Block form"){
      MAX.times{ Sys::ProcTable.ps{} }
   }

   bench.report("Non-block form"){
      MAX.times{ Sys::ProcTable.ps }
   }
end
