require 'rubygems'
require 'rubygems/package_task'
require 'rubygems/tasks'
require 'rbconfig'
require 'rake/clean'

USE_RAKE_COMPILER = (RUBY_PLATFORM =~ /java/) ? false : true
if USE_RAKE_COMPILER
  gem 'rake-compiler', '>=0.6.0'
  require 'rake/extensiontask'
end

require 'date'
require 'fileutils'
require 'rbconfig'


LIBEXT = case RbConfig::CONFIG['host_os'].downcase
  when /darwin/
    "dylib"
  when /mswin|mingw/
    "dll"
  else
    RbConfig::CONFIG['DLEXT']
  end

CPU = case RbConfig::CONFIG['host_cpu'].downcase
  when /i[3456]86/
    # Darwin always reports i686, even when running in 64bit mode
    if RbConfig::CONFIG['host_os'] =~ /darwin/ && 0xfee1deadbeef.is_a?(Fixnum)
      "x86_64"
    else
      "i386"
    end

  when /amd64|x86_64/
    "x86_64"

  when /ppc64|powerpc64/
    "powerpc64"

  when /ppc|powerpc/
    "powerpc"

  when /^arm/
    "arm"

  else
    RbConfig::CONFIG['host_cpu']
  end

OS = case RbConfig::CONFIG['host_os'].downcase
  when /linux/
    "linux"
  when /darwin/
    "darwin"
  when /freebsd/
    "freebsd"
  when /openbsd/
    "openbsd"
  when /sunos|solaris/
    "solaris"
  when /mswin|mingw/
    "win32"
  else
    RbConfig::CONFIG['host_os'].downcase
  end

GMAKE = system('which gmake >/dev/null') && 'gmake' || 'make'

LIBTEST = "build/libtest.#{LIBEXT}"
BUILD_DIR = "build"
BUILD_EXT_DIR = File.join(BUILD_DIR, "#{RbConfig::CONFIG['arch']}", 'ffi_c', RUBY_VERSION)

def gem_spec
  @gem_spec ||= Gem::Specification.load('ffi.gemspec')
end

TEST_DEPS = [ LIBTEST ]
if RUBY_PLATFORM == "java"
  desc "Run all specs"
  task :specs => TEST_DEPS do
    sh %{#{Gem.ruby} -w -S rspec #{Dir["spec/ffi/*_spec.rb"].join(" ")} -fs --color}
  end
  desc "Run rubinius specs"
  task :rbxspecs => TEST_DEPS do
    sh %{#{Gem.ruby} -w -S rspec #{Dir["spec/ffi/rbx/*_spec.rb"].join(" ")} -fs --color}
  end
else
  TEST_DEPS.unshift :compile
  desc "Run all specs"
  task :specs => TEST_DEPS do
    ENV["MRI_FFI"] = "1"
    sh %{#{Gem.ruby} -w -Ilib -I#{BUILD_EXT_DIR} -S rspec #{Dir["spec/ffi/*_spec.rb"].join(" ")} -fs --color}
  end
  desc "Run rubinius specs"
  task :rbxspecs => TEST_DEPS do
    ENV["MRI_FFI"] = "1"
    sh %{#{Gem.ruby} -w -Ilib -I#{BUILD_EXT_DIR} -S rspec #{Dir["spec/ffi/rbx/*_spec.rb"].join(" ")} -fs --color}
  end
end

desc "Build all packages"
task :package => 'gem:package'


CLOBBER.include 'build'
CLOBBER.include FileList['lib/**/ffi_c.so']
CLOBBER.include FileList["lib/**/ffi_c.#{RbConfig::CONFIG['DLEXT']}"]
CLOBBER.include 'lib/ffi/types.conf'
CLOBBER.include 'conftest.dSYM'
CLOBBER.include 'pkg'

task :distclean => :clobber

desc "Build the native test lib"
file "build/libtest.#{LIBEXT}" => FileList['libtest/**/*.[ch]'] do
  sh %{#{GMAKE} -f libtest/GNUmakefile CPU=#{CPU} OS=#{OS} }
end


desc "Build test helper lib"
task :libtest => "build/libtest.#{LIBEXT}"

desc "Test the extension"
task :test => [ :specs, :rbxspecs ]


namespace :bench do
  ITER = ENV['ITER'] ? ENV['ITER'].to_i : 100000
  bench_libs = "-Ilib -I#{BUILD_DIR}" unless RUBY_PLATFORM == "java"
  bench_files = Dir["bench/bench_*.rb"].reject { |f| f == "bench_helper.rb" }
  bench_files.each do |bench|
    task File.basename(bench, ".rb")[6..-1] => TEST_DEPS do
      sh %{#{Gem.ruby} #{bench_libs} #{bench} #{ITER}}
    end
  end
  task :all => TEST_DEPS do
    bench_files.each do |bench|
      sh %{#{Gem.ruby} #{bench_libs} #{bench}}
    end
  end
end

task 'spec:run' => TEST_DEPS
task 'spec:specdoc' => TEST_DEPS

task :default => :specs

task 'gem:win32' do
  sh("rake cross native gem RUBY_CC_VERSION='1.8.7:1.9.3:2.0.0'") || raise("win32 build failed!")
end


namespace 'java' do

  java_gem_spec = Gem::Specification.new do |s|
    s.name = gem_spec.name
    s.version = gem_spec.version
    s.author = gem_spec.author
    s.email = gem_spec.email
    s.homepage = gem_spec.homepage
    s.summary = gem_spec.summary
    s.description = gem_spec.description
    s.files = %w(LICENSE COPYING README.md Rakefile)
    s.has_rdoc = false
    s.license = gem_spec.license
    s.platform = 'java'
  end

  Gem::PackageTask.new(java_gem_spec) do |pkg|
    pkg.need_zip = true
    pkg.need_tar = true
    pkg.package_dir = 'pkg'
  end
end

task 'gem:java' => 'java:gem'


if USE_RAKE_COMPILER
  Rake::ExtensionTask.new('ffi_c', gem_spec) do |ext|
    ext.name = 'ffi_c'                                        # indicate the name of the extension.
    # ext.lib_dir = BUILD_DIR                                 # put binaries into this folder.
    ext.tmp_dir = BUILD_DIR                                   # temporary folder used during compilation.
    ext.cross_compile = true                                  # enable cross compilation (requires cross compile toolchain)
    ext.cross_platform = %w[i386-mingw32 x64-mingw32]                     # forces the Windows platform instead of the default one
  end

  ENV['RUBY_CC_VERSION'].to_s.split(':').each do |ruby_version|
    task "copy:ffi_c:i386-mingw32:#{ruby_version}" do |t|
      %w[lib #{BUILD_DIR}/i386-mingw32/stage/lib].each do |dir| 
        if File.exists?("#{dir}/#{ruby_version[/^\d+\.\d+/]}/ffi_c.so")
          sh "i686-w64-mingw32-strip -S #{dir}/#{ruby_version[/^\d+\.\d+/]}/ffi_c.so"
        end
      end
    end

    task "copy:ffi_c:x64-mingw32:#{ruby_version}" do |t|
      if File.exists?("#{BUILD_DIR}/x64-mingw32/stage/lib/#{ruby_version[/^\d+\.\d+/]}/ffi_c.so")
        sh "x86_64-w64-mingw32-strip -S #{BUILD_DIR}/x64-mingw32/stage/lib/#{ruby_version[/^\d+\.\d+/]}/ffi_c.so"
      end
    end
  end
end

Gem::Tasks.new do |t|
  t.scm.tag.format = '%s'
end

begin
  require 'yard'

  namespace :doc do
    YARD::Rake::YardocTask.new do |yard|
    end
  end
rescue LoadError
  warn "[warn] YARD unavailable"
end
