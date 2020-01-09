# encoding: utf-8
require 'rake'
require 'rake/testtask'
require 'rake/clean'

require_relative "../../rakelib/utils.rb"
require_relative "./rakelib/runtime_dependencies.rb"
require_relative "./rakelib/i18n-tool.rb"

#require 'bundler/gem_tasks'
#require 'runtime_dependencies'

namespace "server" do
  desc "Build Sonic Pi Server"
  task :build, [:make_jobs, :portable, :deployment] => [
    "server:runtime_deps:create_build_dir",
    "server:runtime_deps:build_osmid",
    "build_erlang_scheduler",
    "bundle_gems",
    "patch_aubio_gem",
    "server:translate_tutorial_all_languages"
  ] do |t, args|
    args.with_defaults(:make_jobs => 1)
    if (OS == :windows || OS == :macos)
      args.with_defaults(:portable => true)
    else
      args.with_defaults(:portable => false)
    end

    if (args.portable == true)
      package_runtime_deps
    end
  end

  desc "Configure"
  task :configure, [] => [:patch_erlang_files] do |t, args|

    args.with_defaults(:make_jobs => 1)
    if (OS == :windows || OS == :macos)
      args.with_defaults(:portable => true)
    else
      args.with_defaults(:portable => false)
    end

    if (args.portable == true)
      package_runtime_deps
    end
  end

  Rake::TestTask.new(:test) do |t|
    t.libs << 'test'
    t.pattern = "#{__dir__}/ruby/test/**/test_*.rb"
    t.verbose = true
    # Run setup script when run
    t.ruby_opts = ["-I #{SPI_SERVER_PATH}/ruby/test/", '-r setup_test.rb']
  end

  file "#{SPI_SERVER_PATH}/erlang/osc.beam" => ["#{SPI_SERVER_PATH}/erlang/osc.erl"] do |t|
    exec_sh("erlc #{t.source}")
  end

  file "#{SPI_SERVER_PATH}/erlang/pi_server.beam" => ["#{SPI_SERVER_PATH}/erlang/pi_server.erl"] do |t|
    exec_sh("erlc #{t.source}")
  end

  desc "Build Sonic Pi Erlang scheduler"
  task :build_erlang_scheduler => ["#{SPI_SERVER_PATH}/erlang/osc.beam", "#{SPI_SERVER_PATH}/erlang/pi_server.beam"]

  desc "Patch the Sonic Pi Erlang scheduler code"
  task :patch_erlang_files do
    #install_packages(["erlang"], SPI_BUILD_CONFIG.pkg_manager) if (all_dependencies_installed == false)
    info("Patching Erlang files...")
    #exec_sh(%Q(cd "app/server/erlang"))
    # The current implementation of osc.erl uses Erlang features that require
    # at least Erlang 19.1 to be installed. 16.04 LTS is currently at 18.3.
    # If versions < 19.1 are installed, and we use the current code, the MIDI
    # implementation breaks because the Erlang OSC router is failing.
    ERLANG_VERSION = exec_sh(%Q(#{SPI_SERVER_PATH}/erlang/print_erlang_version))

    if (File.exist?(File.join(SPI_SERVER_PATH, "erlang","osc.erl.orig")))
      # Handle, if the original file in the source tree ever gets updated.
      FileUtils.rm("#{SPI_SERVER_PATH}/erlang/osc.erl.orig")
      exec_sh(%Q(git checkout #{SPI_SERVER_PATH}/erlang/osc.erl))
    end

    # Patch the source code to work with versions of erlang < 19.1 if needed
    if (compare_versions(ERLANG_VERSION, "<", "19.1"))
      info("Found Erlang version < 19.1 (#{ERLANG_VERSION})! Updating source code.")
      exec_sh_commands([
        %Q(cd #{SPI_SERVER_PATH}/erlang/),
        %Q(sed -i.orig 's|erlang:system_time(nanosecond)|erlang:system_time(nano_seconds)|' osc.erl)
      ])
    end
  end

  desc "Bundle Sonic Pi ruby server extensions"
  task :bundle_gems, [:deployment, :path] do |t, args|
    args.with_defaults(:deployment => false)
    args.with_defaults(:path => File.expand_path("#{SPI_SERVER_PATH}/ruby/vendor/bundle"))

    info("Bundling gems and extensions used by the server")

    if (args.deployment == true)
      info("Running 'bundle install --deployment --path=#{File.expand_path(args.path)}'...")
      FileUtils.mkdir_p(args.path)
      sh "cd #{SPI_SERVER_PATH}/ruby && bundle install --deployment --path=#{File.expand_path(args.path)}"
    else
      info("Running 'bundle install --path=#{File.expand_path(args.path)}'...")
      FileUtils.mkdir_p(args.path)
      sh "cd #{SPI_SERVER_PATH}/ruby && bundle install --path=#{File.expand_path(args.path)}"
    end
  end

  # Temporary patch replacing the aubio gem's hard coded path to libaubio
  task :patch_aubio_gem do
    file_path = File.join(SPI_SERVER_PATH, "ruby/vendor/bundle/ruby/#{RUBY_API}/gems/aubio-0.3.1/lib/aubio/aubio-ffi.rb")
    text = ""

    File.foreach(file_path).with_index do |line, line_num|
      if (line_num == 6)
        # Insert patch at line 7 based from https://github.com/xavriley/ruby-aubio/pull/1 by glenpike
        text += %Q(lib_paths = Array(ENV["AUBIO_LIB"] || Dir["/{opt,usr}/{,local/}{lib,lib64,Cellar/aubio**}/libaubio.{*.dylib,so.*}"])\nfallback_names = %w(libaubio.4.2.2.dylib libaubio.so.1 aubio1.dll)\nffi_lib(lib_paths + fallback_names)\n)
      else
        text += line
      end
    end

    f = File.open(file_path, "w")
    f.write(text)
    f.close()
  end

  task :clobber do
    FileUtils.rm(FileList["#{SPI_SERVER_PATH}/erlang/*.beam"])
  end
end
