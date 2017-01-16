require 'rake'
require 'rake/clean'
require 'rake/testtask'
require 'rbconfig'
include RbConfig

CLEAN.include(
  '**/*.core',              # Core dump files
  '**/*.gem',               # Gem files
  '**/*.rbc',               # Rubinius
  '**/*.rbx',               # Rubinius
  '**/*.o',                 # C object file
  '**/*.log',               # Ruby extension build log
  '**/Makefile',            # C Makefile
  '**/conftest.dSYM',       # OS X build directory
  "**/*.#{CONFIG['DLEXT']}" # C shared object
)

desc 'Build the sys-proctable library for C versions of sys-proctable'
task :build => [:clean] do
  if RUBY_PLATFORM == 'java'
    if ENV['JRUBY_OPTS']
      ENV['JRUBY_OPTS'] += ' -Xcext.enabled=true'
    else
      ENV['JRUBY_OPTS'] = '-Xcext.enabled=true'
    end
  end

  case CONFIG['host_os']
    when /hpux/i
      dir = 'ext/hpux'
      ext = '.sl'
  end

  if CONFIG['host_os'] =~ /hpux/i
    Dir.chdir(dir) do
      ruby 'extconf.rb'
      sh 'make'
      cp 'proctable' + ext, 'sys'
    end
  end
end

desc 'Install the sys-proctable library'
task :install => [:build] do
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
    when /hpux/i
      Dir.chdir('ext/hpux'){ sh 'make install' }
  end

  cp(file, dir, :verbose => true) if file
end

desc 'Uninstall the sys-proctable library'
task :uninstall do
  case CONFIG['host_os']
    when /hpux/i
      dir  = File.join(CONFIG['sitearchdir'], 'sys')
      file = File.join(dir, 'proctable.' + CONFIG['DLEXT'])
    else
      dir  = File.join(CONFIG['sitelibdir'], 'sys')
      file = File.join(dir, 'proctable.rb')
  end

  rm(file) 
end

desc 'Run the benchmark suite'
task :bench => [:build] do
  sh "ruby -Ilib benchmarks/bench_ps.rb"
end

desc 'Run the example program'
task :example => [:build] do
  sh 'ruby -Ilib -Iext examples/example_ps.rb'
end

desc 'Run the test suite'
Rake::TestTask.new do |t|
  task :test => :build
  t.libs << 'test' << '.'
   
  case CONFIG['host_os']
    when /mswin|msdos|cygwin|mingw|windows/i
      t.test_files = FileList['test/test_sys_proctable_windows.rb']
      t.libs << 'lib/windows'
    when /linux/i
      t.test_files = FileList['test/test_sys_proctable_linux.rb']
      t.libs << 'lib/linux'
    when /sunos|solaris/i
      t.test_files = FileList['test/test_sys_proctable_sunos.rb']
      t.libs << 'lib/sunos'
    when /aix/i
      t.test_files = FileList['test/test_sys_proctable_aix.rb']
      t.libs << 'lib/aix'
    when /freebsd/i
      t.test_files = FileList['test/test_sys_proctable_freebsd.rb']  
      t.libs << 'lib/freebsd'
    when /darwin/i
      t.libs << 'lib/darwin'
      t.test_files = FileList['test/test_sys_proctable_darwin.rb']
    when /hpux/i
      t.libs << 'ext/hpux'
      t.test_files = FileList['test/test_sys_proctable_hpux.rb']  
  end
end

namespace :gem do
  desc 'Create a gem for the specified OS, or your current OS by default'
  task :create, [:os] => [:clean] do |_task, args|
    require 'rubygems/package'

    if args.is_a?(String)
      os = args
    else
      args.with_defaults(:os => CONFIG['host_os'])
      os = args[:os]
    end

    spec = eval(IO.read('sys-proctable.gemspec'))
    spec.files += ['lib/sys-proctable.rb']

    # I've had to manually futz with the spec here in some cases
    # in order to get the universal platform settings I want because
    # of some bugginess in Rubygems' platform.rb.
    #
    case os
      when /freebsd/i
         spec.platform = Gem::Platform.new(['universal', 'freebsd'])
         spec.require_paths = ['lib', 'lib/freebsd']
         spec.files += ['lib/freebsd/sys/proctable.rb']
         spec.test_files << 'test/test_sys_proctable_freebsd.rb'
         spec.add_dependency('ffi')
      when /darwin/i
         spec.platform = Gem::Platform.new(['universal', 'darwin'])
         spec.require_paths = ['lib', 'lib/darwin']
         spec.files += ['lib/darwin/sys/proctable.rb']
         spec.test_files << 'test/test_sys_proctable_darwin.rb'
         spec.add_dependency('ffi')
      when /hpux/i
         spec.platform = Gem::Platform.new(['universal', 'hpux'])
         spec.files << 'ext/hpux/sys/proctable.c'
         spec.extra_rdoc_files << 'ext/hpux/sys/proctable.c'
         spec.test_files << 'test/test_sys_proctable_hpux.rb'
         spec.extensions = ['ext/hpux/extconf.rb']
      when /linux/i
         spec.platform = Gem::Platform.new(['universal', 'linux'])
         spec.require_paths = ['lib', 'lib/linux']
         spec.files += ['lib/linux/sys/proctable.rb', 'lib/linux/sys/proctable/cgroup_entry.rb', 'lib/linux/sys/proctable/smaps.rb']
         spec.test_files << 'test/test_sys_proctable_linux.rb'
      when /sunos|solaris/i
         spec.platform = Gem::Platform.new(['universal', 'solaris'])
         spec.require_paths = ['lib', 'lib/sunos']
         spec.files += ['lib/sunos/sys/proctable.rb']
         spec.test_files << 'test/test_sys_proctable_sunos.rb'
      when /aix/i
         spec.platform = Gem::Platform.new(['universal', 'aix5'])
         spec.require_paths = ['lib', 'lib/aix']
         spec.files += ['lib/aix/sys/proctable.rb']
         spec.test_files << 'test/test_sys_proctable_aix.rb'
      when /mswin|win32|dos|cygwin|mingw|windows/i
         spec.platform = Gem::Platform.new(['universal', 'mingw32'])
         spec.require_paths = ['lib', 'lib/windows']
         spec.files += ['lib/windows/sys/proctable.rb']
         spec.test_files << 'test/test_sys_proctable_windows.rb'
      else
         raise "Unsupported platform: #{os}"
    end

    spec.test_files << 'test/test_sys_top.rb'
 
    # https://github.com/rubygems/rubygems/issues/147
    spec.original_platform = spec.platform

    spec.signing_key = File.join(Dir.home, '.ssh', 'gem-private_key.pem')

    Gem::Package.build(spec, true)
  end

  desc 'Create a gem for each supported OS'
  task :create_all => [:clean] do
    platforms = %w[aix darwin freebsd hpux linux solaris windows]
    Rake::Task["clean"].execute
    platforms.each{ |os|
      FileUtils.mkdir_p("pkg/#{os}")
      Rake::Task["gem:create"].execute(os)
      Dir.glob("*.gem").each{ |gem| FileUtils.mv(gem, "pkg/#{os}") }
    }
  end

  desc 'Push all gems for each supported OS'
  task :push_all do
    Dir["pkg/**/*.gem"].each{ |file|
      sh "gem push #{file}"
    }
  end

  desc 'Install the sys-proctable library as a gem'
  task :install => [:create] do
    gem_name = Dir['*.gem'].first
    sh "gem install -l #{gem_name}"
  end
end

task :default => :test
