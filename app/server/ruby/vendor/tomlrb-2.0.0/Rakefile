require "bundler/gem_tasks"
require 'rake/testtask'

Rake::TestTask.new do |t|
  t.libs << "test"
  t.test_files = FileList['test/test*.rb']
  t.verbose = true
end
task test: :compile

task default: [:test]

task :compile do
  sh 'racc lib/tomlrb/parser.y -o lib/tomlrb/generated_parser.rb'
end
