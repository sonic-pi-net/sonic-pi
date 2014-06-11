require 'bundler/gem_tasks'
require 'rake/testtask'

task :default => [:test]

Rake::TestTask.new(:test) do |t|
  t.libs << 'lib'
  t.libs << 'test'
  t.pattern = "#{File.dirname(__FILE__)}/test/all.rb"
  t.verbose = true
  t.warning = true
end
Rake::Task['test'].comment = "Run all i18n tests"
