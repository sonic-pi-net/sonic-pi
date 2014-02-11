#!/usr/bin/env rake
require "bundler/gem_tasks"
require "rspec/core/rake_task"
require "rubocop/rake_task"
require "yard"
require "pathname"

ENV["COVERALLS_NOISY"] = "true"

HAMSTER_ROOT = Pathname.new(__FILE__).dirname

desc "Check all files for style guidelines"
Rubocop::RakeTask.new

desc "Run all the tests in spec/"
RSpec::Core::RakeTask.new(:spec) do |config|
  config.verbose = false
end

desc "Generate all of the docs"
YARD::Rake::YardocTask.new do |config|
  config.files = Dir["lib/**/*.rb"]
end

def bench_suites
  Dir[ HAMSTER_ROOT.join('bench/*') ]
    .map(&method(:Pathname))
    .select(&:directory?)
end

def bench_files(suite)
  Dir[File.join(suite, '/**/*.rb')].map(&method(:Pathname))
end

def bench_task_name(file_name)
  file_name.relative_path_from(HAMSTER_ROOT)
           .sub(/\_bench.rb$/, '')
           .to_s
           .tr('/', ':')
end

bench_suites.each do |suite|
  bench_files(suite).each do |bench_file|
    name = bench_task_name(bench_file)

    desc "Benchmark #{name}"
    task name do
      begin
        $LOAD_PATH.unshift HAMSTER_ROOT.join('lib')
        load bench_file
      rescue LoadError => e
        if e.message == /benchmark\/ips/
          $stderr.puts "Please install the benchmark-ips gem"
        else
          $stderr.puts e
        end
        exit 69
      end
    end
  end

  desc "Benchmark #{bench_task_name(suite)}"
  task bench_task_name(suite) => bench_files(suite).map(&method(:bench_task_name))
end

desc "Run all benchmarks"
task bench: bench_suites.map(&method(:bench_task_name))

desc "Default: run tests and generate docs"
task default: [ :spec, :yard, :rubocop ]
