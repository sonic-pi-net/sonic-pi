require 'benchmark'

RSpec::Matchers.define :be_benchmark_results do
  match do |actual|
    actual.is_a? Benchmark::Tms
  end
end
