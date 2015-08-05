#!/usr/bin/env rake

$:.push File.join(File.dirname(__FILE__), 'lib')

require 'concurrent/version'
require 'concurrent/utility/native_extension_loader'

## load the gemspec files
CORE_GEMSPEC = Gem::Specification.load('concurrent-ruby.gemspec')
EXT_GEMSPEC  = Gem::Specification.load('concurrent-ruby-ext.gemspec')
EDGE_GEMSPEC = Gem::Specification.load('concurrent-ruby-edge.gemspec')

## constants used for compile/build tasks

GEM_NAME      = 'concurrent-ruby'
EXT_NAME      = 'extension'
EDGE_NAME     = 'edge'
JAVA_EXT_NAME = 'concurrent_ruby_ext'

if Concurrent.on_jruby?
  CORE_GEM   = "#{GEM_NAME}-#{Concurrent::VERSION}-java.gem"
else
  CORE_GEM   = "#{GEM_NAME}-#{Concurrent::VERSION}.gem"
  EXT_GEM    = "#{GEM_NAME}-ext-#{Concurrent::VERSION}.gem"
  NATIVE_GEM = "#{GEM_NAME}-ext-#{Concurrent::VERSION}-#{Gem::Platform.new(RUBY_PLATFORM)}.gem"
  EDGE_GEM   = "#{GEM_NAME}-edge-#{Concurrent::EDGE_VERSION}.gem"
end

## safely load all the rake tasks in the `tasks` directory

def safe_load(file)
  begin
    load file
  rescue LoadError => ex
    puts "Error loading rake tasks from '#{file}' but will continue..."
    puts ex.message
  end
end

Dir.glob('tasks/**/*.rake').each do |rakefile|
  safe_load rakefile
end

def has_docker?
  system("docker version > /dev/null 2>&1 || boot2docker version > /dev/null 2>&1")
end

if Concurrent.on_jruby?

  ## create the compile task for the JRuby-specific gem
  require 'rake/javaextensiontask'

  Rake::JavaExtensionTask.new(JAVA_EXT_NAME, CORE_GEMSPEC) do |ext|
    ext.ext_dir = 'ext'
  end

elsif Concurrent.allow_c_extensions?

  ## create the compile tasks for the extension gem
  require 'rake/extensiontask'

  Rake::ExtensionTask.new(EXT_NAME, EXT_GEMSPEC) do |ext|
    ext.ext_dir = 'ext/concurrent'
    ext.lib_dir = 'lib/concurrent'
    ext.source_pattern = '*.{c,h}'
    ext.cross_compile = true
    ext.cross_platform = ['x86-mingw32', 'x64-mingw32']
  end

  ENV['RUBY_CC_VERSION'].to_s.split(':').each do |ruby_version|
    platforms = {
      'x86-mingw32' => 'i686-w64-mingw32',
      'x64-mingw32' => 'x86_64-w64-mingw32'
    }
    platforms.each do |platform, prefix|
      task "copy:#{EXT_NAME}:#{platform}:#{ruby_version}" do |t|
        ["lib", "tmp/#{platform}/stage/lib/concurrent"].each do |dir|
          so_file = "#{dir}/#{ruby_version[/^\d+\.\d+/]}/#{EXT_NAME}.so"
          if File.exists?(so_file)
            sh "#{prefix}-strip -S #{so_file}"
          end
        end
      end
    end
  end
else
  ## create an empty compile task
  task :compile
end

task :clean do
  rm_rf 'pkg/classes'
  rm_rf 'tmp'
  rm_rf Dir.glob('lib/concurrent/1.?')
  rm_rf Dir.glob('lib/concurrent/2.?')
  rm_f Dir.glob('./**/*.so')
  rm_f Dir.glob('./**/*.bundle')
  rm_f Dir.glob('./lib/*.jar')
  mkdir_p 'pkg'
end

## create build tasks tailored to current platform

namespace :build do

  task :mkdir_pkg do
    mkdir_p 'pkg'
  end

  build_deps = [:clean, 'build:mkdir_pkg']
  build_deps << :compile if Concurrent.on_jruby?

  desc "Build #{CORE_GEM} into the pkg directory"
  task :core => build_deps do
    sh "gem build #{CORE_GEMSPEC.name}.gemspec"
    sh 'mv *.gem pkg/'
  end

  unless Concurrent.on_jruby?

    desc "Build #{EDGE_GEM} into the pkg directory"
    task :edge => 'build:mkdir_pkg' do
      sh "gem build #{EDGE_GEMSPEC.name}.gemspec"
      sh 'mv *.gem pkg/'
    end

    desc "Build #{EXT_GEM} into the pkg directory"
    task :ext => build_deps do
      sh "gem build #{EXT_GEMSPEC.name}.gemspec"
      sh 'mv *.gem pkg/'
    end
  end

  if Concurrent.allow_c_extensions?
    desc "Build #{NATIVE_GEM} into the pkg directory"
    task :native => 'build:mkdir_pkg' do
      sh "gem compile pkg/#{EXT_GEM}"
      sh 'mv *.gem pkg/'
    end
  end

  if has_docker?
    desc "Build the windows binary #{Concurrent::VERSION} gems per rake-compiler-dock"
    task :windows do
      require 'rake_compiler_dock'
      RakeCompilerDock.sh <<-EOT
      bundle --without="development testing" &&
      rake cross native gem RUBY_CC_VERSION=1.9.3:2.0.0:2.1.6:2.2.2
      rm -rf .bundle
      EOT
    end
  end
end

if Concurrent.on_jruby?
  desc 'Build JRuby-specific core gem (alias for `build:core`)'
  task :build => ['build:core']
elsif has_docker?
  desc 'Build core, extension, and edge gems, including Windows binaries'
  task :build => ['build:core', 'build:ext', 'build:edge', 'build:windows']
else
  desc 'Build core, extension, and edge gems (excluding Windows binaries)'
  task :build => ['build:core', 'build:ext', 'build:edge']
end

## the RSpec task that compiles extensions when available

begin
  require 'rspec'
  require 'rspec/core/rake_task'

  RSpec::Core::RakeTask.new(:spec)

  RSpec::Core::RakeTask.new(:travis) do |t|
    t.rspec_opts = '--color ' \
                   '--backtrace ' \
                   '--tag ~unfinished ' \
                   '--seed 1 ' \
                   '--format documentation ' \
                   '--tag ~notravis'
  end

  RSpec::Core::RakeTask.new(:appveyor) do |t|
    t.rspec_opts = '--backtrace ' \
                   '--tag ~unfinished ' \
                   '--seed 1 ' \
                   '--format documentation ' \
                   '--tag ~notravis'
  end

  if Concurrent.on_windows?
    task :ci => [:clean, :compile, :appveyor]
  else
    task :ci => [:clean, :compile, :travis]
  end

  task :default => [:clean, :compile, :spec]
rescue LoadError
  puts 'Error loading Rspec rake tasks, probably building the gem...'
end
