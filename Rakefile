# Sonic Pi Rakefile
# Builds Sonic Pi for the current platform

# encoding: utf-8
require 'rake'

require_relative "./rakelib/utils"

require_relative "./app/server/Rakefile.rb"
require_relative "./app/gui/qt/Rakefile.rb"

task :default => ["build"]

desc "Build Sonic Pi (default task)"
task :build, [:make_jobs, :dev_build] => [
	"server:build",
	"qt_gui:build_qt_docs",
	"qt_gui:build"
] do |t, args|
	args.with_defaults(:make_jobs => 1)
	args.with_defaults(:dev_build => true)
end

desc "Test Sonic Pi"
task :test, [] => [
	"server:test"
] do |t, args|
end

desc "Install Sonic Pi"
task :install, [:install_prefix, :verbose] => [] do |t, args|
	args.with_defaults(:verbose => false)
	case OS
	when :linux, :raspberry
		#puts("Set default install prefix to /opt/sonic-pi")
		args.with_defaults(:install_prefix => "/opt/sonic-pi")
	when :windows
		# NOT TESTED!
		# args.with_defaults(:install_prefix => "C:\\\\Program Files\\Sonic Pi")
	when :macos
		# NOT TESTED!
		# args.with_defaults(:install_prefix => "/Applications/Sonic Pi.app")
	end

	install_prefix = File.expand_path(args.install_prefix)

	info("Install prefix: #{install_prefix}")
	if (File.directory?(install_prefix))
		if ask_yes_no("The install prefix: '#{install_prefix}' already exists. Do you want to delete it?", default="no")
			FileUtils.rm_rf(install_prefix)
			FileUtils.mkdir_p(install_prefix)
		elsif !(ask_yes_no("Continue installation? Some files in the folder may be over written, and the installation may fail due to protected files.", default="no"))
			abort("Aborting installation")
		end
	else
		FileUtils.mkdir_p(install_prefix)
	end

	server_install_files  = Dir["#{__dir__}/app/server/erlang/*.beam"] # Compiled erlang files
	server_install_files += Dir["#{__dir__}/app/server/native/**/*.*"] # Native binaries
	server_install_files += Dir["#{__dir__}/app/server/ruby/bin/*.rb"] # Main server ruby files
	server_install_files += Dir["#{__dir__}/app/server/ruby/lib/**/*.rb"] # Sonic Pi server lib files
	server_install_files += Dir["#{__dir__}/app/server/ruby/vendor/**/*.*"] # Bundled gems used by the ruby server
	server_install_files += [
		"#{__dir__}/app/server/ruby/core.rb",
		"#{__dir__}/app/server/ruby/util.rb",
		"#{__dir__}/app/server/ruby/Gemfile.lock"
	]

	gui_install_files		 = ["#{__dir__}/app/gui/qt/sonic-pi"] # Qt GUI binary
	gui_install_files		+= Dir["#{__dir__}/app/gui/qt/theme/**/*.*"] # UI Themes

	share_install_files  = Dir["#{__dir__}/etc/buffers/**/*.*"]
	share_install_files += Dir["#{__dir__}/etc/examples/**/*.*"]
	share_install_files += Dir["#{__dir__}/etc/samples/**/*.*"]
	share_install_files += Dir["#{__dir__}/etc/snippets/**/*.*"]
	share_install_files += Dir["#{__dir__}/etc/synthdefs/**/*.*"]
	share_install_files += [
		"#{__dir__}/CHANGELOG.md",
		"#{__dir__}/COMMUNITY.md",
		"#{__dir__}/CONTRIBUTORS.md",
		"#{__dir__}/CORETEAM.html",
		"#{__dir__}/LICENSE.md"
	]

	info("Installing server files...")
	install_files(server_install_files, SONIC_PI_ROOT, install_prefix, args.verbose)
	info("Installing Qt GUI files...")
	install_files(gui_install_files, SONIC_PI_ROOT, install_prefix, args.verbose)
	info("Installing other shared data...")
	install_files(share_install_files, SONIC_PI_ROOT, install_prefix, args.verbose)

	info("Installation done! Installed to #{install_prefix}")
end
