
class NopClass
  def nop
  end
end

nop = NopClass.new
fal = nil

n = 1000_000
require 'benchmark'
Benchmark.bm(9) do |bm|
  bm.report(:unless)    { n.times do method_call if fal end }
  bm.report(:nop)       { n.times do nop.nop end }
end