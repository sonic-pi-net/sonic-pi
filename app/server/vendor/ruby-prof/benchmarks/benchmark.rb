require 'ruby-prof'
require 'benchmark'

def go
end

puts Benchmark.realtime {
 RubyProf.profile do
  100000.times { go }
 end
}

for n in [5, 100] do
 n.times { Thread.new { sleep }}
 puts Benchmark.realtime {
  RubyProf.profile do
    100000.times { go }
  end
 }
end
