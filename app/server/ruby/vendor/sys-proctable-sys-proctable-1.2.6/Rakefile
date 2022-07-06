require 'rake'
require 'rake/clean'
require 'rake/testtask'
require 'rbconfig'
require 'rspec/core/rake_task'
include RbConfig

CLEAN.include('**/*.gem', '**/*.rbc')

desc 'Install the sys-proctable library'
task :install do
  file = nil
  dir  = File.join(CONFIG['sitelibdir'], 'sys')

  Dir.mkdir(dir) unless File.exists?(dir)

  case CONFIG['host_os']
    when /mswin|win32|msdos|cygwin|mingw|windows/i
      file = 'lib/windows/sys/proctable.rb'
    when /linux/i
      file = 'lib/linux/sys/proctable.rb'
    when /sunos|solaris/i
      file = 'lib/sunos/sys/proctable.rb'
    when /aix/i
      file = 'lib/aix/sys/proctable.rb'
    when /freebsd/i
      file = 'lib/freebsd/sys/proctable.rb'
    when /darwin/i
      file = 'lib/darwin/sys/proctable.rb'
  end

  cp(file, dir, :verbose => true) if file
end

desc 'Uninstall the sys-proctable library'
task :uninstall do
  dir  = File.join(CONFIG['sitelibdir'], 'sys')
  file = File.join(dir, 'proctable.rb')
  rm(file) 
end

desc 'Run the benchmark suite'
task :bench do
  sh "ruby -Ilib benchmarks/bench_ps.rb"
end

desc 'Run the example program'
task :example do
  sh 'ruby -Ilib -Iext examples/example_ps.rb'
end

desc 'Run the test suite for the sys-proctable library'
RSpec::Core::RakeTask.new(:spec) do |t|
  t.pattern = ['spec/sys_proctable_all_spec.rb']

  case CONFIG['host_os']
    when /aix/i
      t.rspec_opts = '-Ilib/aix'
      t.pattern << 'spec/sys_proctable_aix.rb'
    when /darwin/i
      t.rspec_opts = '-Ilib/darwin'
      t.pattern << 'spec/sys_proctable_darwin_spec.rb'
    when /freebsd/i
      t.rspec_opts = '-Ilib/freebsd'
      t.pattern << 'spec/sys_proctable_freebsd_spec.rb'
    when /linux/i
      t.rspec_opts = '-Ilib/linux'
      t.pattern << 'spec/sys_proctable_linux_spec.rb'
    when /sunos|solaris/i
      t.rspec_opts = '-Ilib/sunos'
      t.pattern << 'spec/sys_proctable_sunos_spec.rb'
    when /mswin|msdos|cygwin|mingw|windows/i
      t.rspec_opts = '-Ilib/windows'
      t.pattern << 'spec/sys_proctable_windows_spec.rb'
  end
end

namespace :gem do
  desc 'Create a gem for the specified OS, or your current OS by default'
  task :create => [:clean] do
    require 'rubygems/package'
    spec = eval(IO.read('sys-proctable.gemspec'))
    spec.signing_key = File.join(Dir.home, '.ssh', 'gem-private_key.pem')
    Gem::Package.build(spec, true)
  end

  desc 'Install the sys-proctable library as a gem'
  task :install => [:create] do
    gem_name = Dir['*.gem'].first
    sh "gem install -l #{gem_name}"
  end
end

task :default => :spec
