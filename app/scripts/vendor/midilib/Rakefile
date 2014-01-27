require 'rubygems'
require 'rake'
if RUBY_VERSION >= '1.9'
  require 'rdoc/task'
  require 'rubygems/package_task'
  require 'rake/testtask'
else
require 'rake/rdoctask'
  require 'rake/gempackagetask'
  require 'rake/runtest'
end

PROJECT_NAME = 'midilib'
RDOC_DIR = 'html'

PKG_FILES = FileList[ 'ChangeLog', 'Credits', 'Rakefile',
    'README.rdoc', 'TODO.rdoc',
    'examples/**/*',
    'html/**/*',
    'install.rb',
    'lib/**/*.rb',
    'test/**/*']

task :default => [:package]

spec = Gem::Specification.new do |s|
    s.platform = Gem::Platform::RUBY
    s.name = PROJECT_NAME
    s.version = `ruby -Ilib -e 'require "midilib/info"; puts MIDI::Version'`.strip
    s.requirements << 'none'

    s.require_path = 'lib'

    s.files = PKG_FILES.to_a

    s.has_rdoc = true
    s.rdoc_options << '--main' << 'README.rdoc'
    s.extra_rdoc_files = ['README.rdoc', 'TODO.rdoc']

    s.author = 'Jim Menard'
    s.email = 'jim@jimmenard.com'
    s.homepage = 'http://github.com/jimm/midilib'
    s.rubyforge_project = PROJECT_NAME

    s.summary = "MIDI file and event manipulation library"
    s.description = <<EOF
midilib is a pure Ruby MIDI library useful for reading and
writing standard MIDI files and manipulating MIDI event data.
EOF
end

if RUBY_VERSION >= '1.9'
  # Creates a :package task (also named :gem). Also useful are
  # :clobber_package and :repackage.
  Gem::PackageTask.new(spec) do |pkg|
    pkg.need_zip = true
    pkg.need_tar = true
  end
else
  # Creates a :package task (also named :gem). Also useful are
  # :clobber_package and :repackage.
  Rake::GemPackageTask.new(spec) do |pkg|
    pkg.need_zip = true
    pkg.need_tar = true
  end
end

# creates an "rdoc" task
Rake::RDocTask.new do | rd |
    rd.main = 'README.rdoc'
    rd.title = PROJECT_NAME
    rd.rdoc_files.include('README.rdoc', 'TODO.rdoc', 'lib/**/*.rb')
end

desc "Publish gem"
task :publish => [:rdoc, :package] do
  version = `ruby -Ilib -e 'require "midilib/info"; puts MIDI::Version'`.strip
  system "gem push pkg/midilib-#{version}.gem"
end

if RUBY_VERSION >= '1.9'
  Rake::TestTask.new
else
  task :test do
    Rake::run_tests
  end
end

task :clean => [:clobber_rdoc, :clobber_package]
