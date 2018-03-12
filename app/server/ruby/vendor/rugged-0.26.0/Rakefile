require 'rake/testtask'

begin
  require 'rake/extensiontask'
rescue LoadError
  abort <<-error
  rake-compiler is missing; Rugged depends on rake-compiler to build the C wrapping code.

  Install it by running `gem i rake-compiler`
error
end

gemspec = Gem::Specification::load(File.expand_path('../rugged.gemspec', __FILE__))

Gem::PackageTask.new(gemspec) do |pkg|
end

Rake::ExtensionTask.new('rugged', gemspec) do |r|
  r.lib_dir = 'lib/rugged'
end

desc "checkout libgit2 source"
task :checkout do
  if !ENV['CI_BUILD']
    sh "git submodule update --init"
  end
end
Rake::Task[:compile].prerequisites.insert(0, :checkout)

namespace :clean do
  task :libgit2 do
    FileUtils.rm_rf("vendor/libgit2/build")
  end
end
Rake::Task[:clean].prerequisites << "clean:libgit2"

desc "Open an irb session preloaded with Rugged"
task :console do
  exec "script/console"
end

#
# Tests
#
task :default => [:compile, :test]

task :cover do
  ruby 'test/coverage/cover.rb'
end

Rake::TestTask.new do |t|
  t.libs << 'lib' << 'test'
  t.pattern = 'test/**/*_test.rb'
  t.verbose = false
  t.warning = true
end

begin
  require 'rdoc/task'
  Rake::RDocTask.new do |rdoc|
    rdoc.rdoc_dir = 'rdoc'
    rdoc.rdoc_files.include('ext/**/*.c')
    rdoc.rdoc_files.include('lib/**/*.rb')
  end
rescue LoadError
end

