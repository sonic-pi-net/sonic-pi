require "bundler/gem_tasks"

require "rake/testtask"
Rake::TestTask.new(:test) do |t|
  t.libs << "test"
  t.libs << "lib"
  t.test_files = FileList['test/**/*_test.rb']
end

task :default => :spec

require 'rake/extensiontask'
spec = Gem::Specification.load('fast_osc.gemspec')
Rake::ExtensionTask.new('fast_osc') do |ext|
  # ext.platform or ext.cross_config_options
  # might work to enable universal builds on darwin for older processors
  ext.lib_dir = "lib/fast_osc"
end
