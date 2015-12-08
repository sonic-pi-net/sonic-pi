# encoding: utf-8
require_relative './test/setup_test.rb'
require 'rake'

require 'rake/testtask'

task :default => :test
Rake::TestTask.new do |t|
  t.libs << 'test'
  t.pattern = 'test/**/test_*.rb'
  t.verbose = true
end
