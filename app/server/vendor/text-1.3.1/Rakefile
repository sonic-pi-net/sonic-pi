require 'rake'
require 'rake/testtask'

Rake::TestTask.new do |t|
  t.libs << 'test'
  t.test_files = FileList['test/*_test.rb']
  t.verbose = false
end

desc "Run benchmark"
task :benchmark do |t|
  system "ruby -v"
  system "ruby perf/benchmark.rb"
end

task :default => :test
