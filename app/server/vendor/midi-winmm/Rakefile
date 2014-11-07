
require 'rake'
require 'rake/testtask'

Rake::TestTask.new(:test) do |t|

  t.libs << "test"
  t.test_files = FileList["test/**/test_*.rb"]
  t.verbose = true
end

task :default => [:test]