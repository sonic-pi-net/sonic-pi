require "rbconfig"

require 'rake/baseextensiontask'

# Define a series of tasks to aid in the compilation of Java extensions for
# gem developer/creators.

module Rake
  class JavaExtensionTask < BaseExtensionTask

    attr_accessor :classpath
    attr_accessor :debug

    # Provide source compatibility with specified release
    attr_accessor :source_version

    # Generate class files for specific VM version
    attr_accessor :target_version

    attr_accessor :encoding

    # Specify lint option
    attr_accessor :lint_option

    def platform
      @platform ||= 'java'
    end

    def java_compiling(&block)
      @java_compiling = block if block_given?
    end

    def init(name = nil, gem_spec = nil)
      super
      @source_pattern = '**/*.java'
      @classpath      = nil
      @debug          = false
      @source_version = '1.6'
      @target_version = '1.6'
      @encoding       = nil
      @java_compiling = nil
      @lint_option    = nil
    end

    def define
      super

      define_java_platform_tasks
    end

    private
    def define_compile_tasks(for_platform = nil, ruby_ver = RUBY_VERSION)
      # platform usage
      platf = for_platform || platform

      # lib_path
      lib_path = lib_dir

      # tmp_path
      tmp_path = "#{@tmp_dir}/#{platf}/#{@name}"

      # cleanup and clobbering
      CLEAN.include(tmp_path)
      CLOBBER.include("#{lib_path}/#{binary(platf)}")
      CLOBBER.include("#{@tmp_dir}")

      # directories we need
      directory tmp_path
      directory lib_dir

      # copy binary from temporary location to final lib
      # tmp/extension_name/extension_name.{so,bundle} => lib/
      task "copy:#{@name}:#{platf}" => [lib_path, "#{tmp_path}/#{binary(platf)}"] do
        install "#{tmp_path}/#{binary(platf)}", "#{lib_path}/#{binary(platf)}"
      end

      file "#{tmp_path}/#{binary(platf)}" => "#{tmp_path}/.build" do

        class_files = FileList["#{tmp_path}/**/*.class"].
          gsub("#{tmp_path}/", '')

        # avoid environment variable expansion using backslash
        class_files.gsub!('$', '\$') unless windows?

        args = class_files.map { |path|
          ["-C #{tmp_path}", path]
        }.flatten

        sh "jar cf #{tmp_path}/#{binary(platf)} #{args.join(' ')}"
      end

      file "#{tmp_path}/.build" => [tmp_path] + source_files do
        not_jruby_compile_msg = <<-EOF
WARNING: You're cross-compiling a binary extension for JRuby, but are using
another interpreter. If your Java classpath or extension dir settings are not
correctly detected, then either check the appropriate environment variables or
execute the Rake compilation task using the JRuby interpreter.
(e.g. `jruby -S rake compile:java`)
        EOF
        warn_once(not_jruby_compile_msg) unless defined?(JRUBY_VERSION)

        javac_command_line = [
          "javac",
          "-target", @target_version,
          "-source", @source_version,
          java_lint_arg,
          "-d", tmp_path,
        ]
        javac_command_line.concat(java_encoding_args)
        javac_command_line.concat(java_extdirs_args)
        javac_command_line.concat(java_classpath_args)
        javac_command_line << "-g" if @debug
        javac_command_line.concat(source_files)
        sh(*javac_command_line)

        # Checkpoint file
        touch "#{tmp_path}/.build"
      end

      # compile tasks
      unless Rake::Task.task_defined?('compile') then
        desc "Compile all the extensions"
        task "compile"
      end

      # compile:name
      unless Rake::Task.task_defined?("compile:#{@name}") then
        desc "Compile #{@name}"
        task "compile:#{@name}"
      end

      # Allow segmented compilation by platform (open door for 'cross compile')
      task "compile:#{@name}:#{platf}" => ["copy:#{@name}:#{platf}"]
      task "compile:#{platf}" => ["compile:#{@name}:#{platf}"]

      # Only add this extension to the compile chain if current
      # platform matches the indicated one.
      if platf == RUBY_PLATFORM then
        # ensure file is always copied
        file "#{lib_path}/#{binary(platf)}" => ["copy:#{name}:#{platf}"]

        task "compile:#{@name}" => ["compile:#{@name}:#{platf}"]
        task "compile" => ["compile:#{platf}"]
      end
    end

    def define_java_platform_tasks
      # lib_path
      lib_path = lib_dir

      if @gem_spec && !Rake::Task.task_defined?("java:#{@gem_spec.name}")
        task "java:#{@gem_spec.name}" do |t|
          # FIXME: workaround Gem::Specification limitation around cache_file:
          # http://github.com/rubygems/rubygems/issues/78
          spec = gem_spec.dup
          spec.instance_variable_set(:"@cache_file", nil) if spec.respond_to?(:cache_file)

          # adjust to specified platform
          spec.platform = Gem::Platform.new('java')

          # clear the extensions defined in the specs
          spec.extensions.clear

          # add the binaries that this task depends on
          ext_files = []

          # go through native prerequisites and grab the real extension files from there
          t.prerequisites.each do |ext|
            ext_files << ext
          end

          # include the files in the gem specification
          spec.files += ext_files

          # expose gem specification for customization
          if @java_compiling
            @java_compiling.call(spec)
          end

          # Generate a package for this gem
          Gem::PackageTask.new(spec) do |pkg|
            pkg.need_zip = false
            pkg.need_tar = false
          end
        end

        # add binaries to the dependency chain
        task "java:#{@gem_spec.name}" => ["#{lib_path}/#{binary(platform)}"]

        # ensure the extension get copied
        unless Rake::Task.task_defined?("#{lib_path}/#{binary(platform)}") then
          file "#{lib_path}/#{binary(platform)}" => ["copy:#{name}:#{platform}"]
        end

        task 'java' => ["java:#{@gem_spec.name}"]
      end

      task 'java' do
        task 'compile' => 'compile:java'
      end
    end

    #
    # Discover Java Extension Directories and build an extdirs arguments
    #
    def java_extdirs_args
      extdirs = Java::java.lang.System.getProperty('java.ext.dirs') rescue nil
      extdirs ||= ENV['JAVA_EXT_DIR']
      if extdirs.nil?
        []
      else
        ["-extdirs", extdirs]
      end
    end

    #
    # Build an encoding arguments
    #
    def java_encoding_args
      if @encoding.nil?
        []
      else
        ["-encoding", @encoding]
      end
    end

    #
    # Discover the Java/JRuby classpath and build a classpath arguments
    #
    # Copied verbatim from the ActiveRecord-JDBC project. There are a small myriad
    # of ways to discover the Java classpath correctly.
    #
    def java_classpath_args
      jruby_cpath = nil
      if RUBY_PLATFORM =~ /java/
        begin
          cpath  = Java::java.lang.System.getProperty('java.class.path').split(File::PATH_SEPARATOR)
          cpath += Java::java.lang.System.getProperty('sun.boot.class.path').split(File::PATH_SEPARATOR)
          jruby_cpath = cpath.compact.join(File::PATH_SEPARATOR)
        rescue => e
        end
      end

      # jruby_cpath might not be present from Java-9 onwards as it removes
      # sun.boot.class.path. Check if JRUBY_HOME is set as env variable and try
      # to find jruby.jar under JRUBY_HOME
      unless jruby_cpath
        jruby_home = ENV['JRUBY_HOME']
        if jruby_home
          candidate = File.join(jruby_home, 'lib', 'jruby.jar')
          jruby_cpath = candidate if File.exist?(candidate)
        end
      end

      # JRUBY_HOME is not necessarily set in JRuby-9.x
      # Find the libdir from RbConfig::CONFIG and find jruby.jar under the
      # found lib path
      unless jruby_cpath
        libdir = RbConfig::CONFIG['libdir']
        if libdir.start_with?("uri:classloader:")
          raise 'Cannot build with jruby-complete from Java 9 onwards'
        end
        candidate = File.join(libdir, "jruby.jar")
        jruby_cpath = candidate if File.exist?(candidate)
      end

      unless jruby_cpath
        raise "Could not find jruby.jar. Please set JRUBY_HOME or use jruby in rvm"
      end

      if @classpath and @classpath.size > 0
        jruby_cpath = [jruby_cpath, *@classpath].join(File::PATH_SEPARATOR)
      end
      ["-cp", jruby_cpath]
    end

    #
    # Convert a `-Xlint:___` linting option such as `deprecation` into a full javac argument, such as `-Xlint:deprecation`.
    #
    # @return [String]              Default: _Simply `-Xlint` is run, which enables recommended warnings.
    #
    def java_lint_arg
      return '-Xlint' unless @lint_option

      "-Xlint:#{@lint_option}"
    end
  end
end
