#!/usr/bin/env rake
require "bundler/gem_tasks"
require "rspec/core/rake_task"
require "yard"
require "pathname"

HAMSTER_ROOT = Pathname.new(__FILE__).dirname

desc "Run all the tests in spec/"
RSpec::Core::RakeTask.new(:spec) do |config|
  config.verbose = false
end

desc "Generate all of the docs"
YARD::Rake::YardocTask.new do |config|
  config.files = Dir["lib/**/*.rb"]
end

def bench_suites
  Dir[HAMSTER_ROOT.join('bench/*')].map(&method(:Pathname)).select(&:directory?)
end

def bench_files(suite)
  Dir[File.join(suite, '/**/*.rb')].map(&method(:Pathname))
end

def bench_task_name(file_name)
  file_name.relative_path_from(HAMSTER_ROOT).sub(/\_bench.rb$/, '').to_s.tr('/', ':')
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

desc "Generate file dependency graph"
task :dependency_graph do
  if `which dot`.empty?
    raise "dot is not installed or not on your system path"
  end

  dependencies = Hash.new { |h,k| h[k] = Set.new }
  trim_fn = ->(fn) { fn.sub(/^lib\//, '').sub(/\.rb$/, '') }

  Dir["lib/**/*.rb"].each do |path|
    File.readlines(path).each do |line|
      if line =~ /^\s*require\s+('|")([^'"]*)('|")/
         dependency = $2
         dependencies[trim_fn[path]] << dependency
      end
    end
  end

  require 'set'
  cycles    = Set.new
  reachable = Hash.new { |h,k| h[k] = Set.new }
  find_reachable = ->(from, to, pathsofar) do
    to.each do |t|
      if t == from
        reachable[from].add(t)
        pathsofar.push(t).each_cons(2) { |vector| cycles << vector }
      elsif reachable[from].add?(t) && dependencies.key?(t)
        find_reachable[from, dependencies[t], pathsofar.dup.push(t)]
      end
    end
  end
  dependencies.each { |from,to| find_reachable[from,to,[from]] }

  dot = %|digraph { graph [label="Hamster srcfile dependencies"]\n|
  dependencies.each do |from,to|
    dot << %|"#{from}" [color=red]\n| if reachable[from].include?(from)
    to.each do |t|
      dot << %|"#{from}" -> "#{t}" #{'[color=red]' if cycles.include?([from,t])}\n|
    end
  end
  dot << "\n}"

  require "tempfile"
  Tempfile.open("hamster-depgraph") do |f|
    f.write(dot)
    f.flush
    message = `dot -Tgif #{f.path} -o depgraph.gif`
    f.unlink
    puts message unless message.empty?
    puts "Dependency graph is in depgraph.gif"
  end
end

desc "Default: run tests and generate docs"
task default: [ :spec, :yard ]
