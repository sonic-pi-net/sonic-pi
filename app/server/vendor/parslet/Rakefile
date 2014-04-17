require 'rdoc/task'
require 'sdoc'

require 'rspec/core/rake_task'
require "rubygems/package_task"

desc "Run all tests: Exhaustive."
RSpec::Core::RakeTask.new

namespace :spec do
  desc "Only run unit tests: Fast. "
  RSpec::Core::RakeTask.new(:unit) do |task|
    task.pattern = "spec/parslet/**/*_spec.rb"
  end
end

task :default => :spec

# This task actually builds the gem. 
task :gem => :spec
spec = eval(File.read('parslet.gemspec'))

desc "Prints LOC stats"
task :stat do
  %w(lib spec example).each do |dir|
    loc = %x(find #{dir} -name "*.rb" | xargs wc -l | grep 'total').split.first.to_i
    printf("%20s %d\n", dir, loc)
  end
end

