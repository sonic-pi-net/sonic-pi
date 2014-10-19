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

## YARD customizations

# In Hamster, we often use "def_delegator :self" to alias methods
# YARD doesn't understand what this does and ignores it
# We will extend YARD so that it can generate documentation for methods created
#   using def_delegator
#
class DefDelegatorHandler < YARD::Handlers::Ruby::Base
  handles method_call(:def_delegator)
  namespace_only

  process do
    arg_list = statement.children[1]
    delegate = eval(arg_list.children[0].source)
    old_meth = eval(arg_list.children[1].source).to_sym
    new_meth = arg_list.children[2]
    new_meth = new_meth ? eval(new_meth.source).to_sym : old_meth

    if delegate == 'self' || delegate == :self
      old_obj = namespace.child(:name => old_meth, :scope => scope)
      new_obj = register YARD::CodeObjects::MethodObject.new(namespace, new_meth, scope) do |o|
        o.add_file(parser.file, statement.line)
      end

      if old_obj
        new_obj.signature = old_obj.signature
        new_obj.source    = old_obj.source
        new_obj.docstring = old_obj.docstring + YARD::Docstring.new(statement.comments)
        new_obj.docstring.line_range = statement.comments_range
        new_obj.docstring.hash_flag  = statement.comments_hash_flag
        new_obj.docstring.object     = new_obj
      else
        new_obj.signature = "def #{new_meth}" # this is all we know.
      end

      namespace.aliases[new_obj] = old_meth
    else
      register YARD::CodeObjects::MethodObject.new(namespace, new_meth, scope)
    end
  end
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

desc "Default: run tests and generate docs"
task default: [ :spec, :yard ]
