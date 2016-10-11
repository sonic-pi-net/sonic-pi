#!/usr/bin/env rake

require "rubygems"
require "bundler/setup"
Bundler::GemHelper.install_tasks

# See https://github.com/colszowka/simplecov/issues/171
desc "Set permissions on all files so they are compatible with both user-local and system-wide installs"
task :fix_permissions do
  system 'bash -c "find . -type f -exec chmod 644 {} \; && find . -type d -exec chmod 755 {} \;"'
end
# Enforce proper permissions on each build
Rake::Task[:build].prerequisites.unshift :fix_permissions

require "rspec/core/rake_task"
RSpec::Core::RakeTask.new(:spec) do |spec|
  spec.pattern = FileList["spec/*_spec.rb"]
end

begin
  require "rubocop/rake_task"
  RuboCop::RakeTask.new
rescue LoadError
  task :rubocop do
    $stderr.puts "Rubocop is disabled"
  end
end

# Cucumber integration test suite is for impls that work with simplecov only - a.k.a. 1.9+
if RUBY_VERSION >= "1.9"
  require "cucumber/rake/task"
  Cucumber::Rake::Task.new
  task :default => [:spec, :cucumber, :rubocop]
else
  task :default => [:spec]
end
