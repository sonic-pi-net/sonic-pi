# encoding: utf-8
require 'rake'
require 'rake/testtask'

task :default => :test
Rake::TestTask.new do |t|
  ruby File.expand_path("#{__dir__}/test/setup_test.rb")
  t.libs << 'test'
  t.pattern = 'test/**/test_*.rb'
  t.verbose = true
end
