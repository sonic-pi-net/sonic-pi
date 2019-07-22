# Sonic Pi Rakefile
# Builds Sonic Pi for the current platform

# encoding: utf-8
require 'rake'

require 'fileutils'
require "./build_scripts/utils"
#require "./build_scripts/runtime_dependencies"
#require "./build_scripts/Dependencies"

require_relative "app/server/Rakefile.rb"
require_relative "app/gui/qt/Rakefile.rb"

RUBY_API = RbConfig::CONFIG['ruby_version']

all_dependencies_installed = false

# task build: %w[install_all_dependency_packages supercollider build_aubio build_osmid build_erlang_files compile_extensions build_documentation build_qt_docs build_gui]
task :default => ["build"]

desc "Build Sonic Pi (default task)"
task :build, [:make_jobs, :sonic_pi_root] => [
	"server:build",
	"qt_gui:build_qt_docs",
	"qt_gui:build"
] do | t, args|
	args.with_defaults(:make_jobs => 1)
  args.with_defaults(:sonic_pi_root => File.join(File.expand_path(Dir.pwd), "build", "sonic-pi"))
end
