# What is rake-compiler?

rake-compiler is first and foremost a productivity tool for Ruby developers.
Its goal is to make the busy developer's life easier by simplifying the building
and packaging of Ruby extensions by simplifying code and reducing duplication.

It follows **convention over configuration** by advocating a standardized build and
package structure for both C and Java based RubyGems.

rake-compiler is the result of many hard-won experiences dealing with several
diverse RubyGems that provided native extensions for different platforms and
different user configurations in different ways. Details such as differences in
code portability, differences in code clarity, and differences in project directory
structure often made it very difficult for newcomers to those RubyGems.

From these challenges, rake-compiler was born with the single-minded goal of
making the busy RubyGem developer's life much less difficult.

## Feature Overview

Some of the benefits rake-compiler provides include:

* No custom rake tasks required. Less code duplication and errors.

* Painlessly build extensions on different platforms (Linux, OSX and Windows).

* Painlessly build extensions for different Ruby implementations (JRuby,
  Rubinius and MRI).

* Allow multiple extensions to be compiled inside the same gem.

* Painlessly build "fat" native gems for Windows users (from Linux or OSX).

* Mimics RubyGems standard installation process, helping as a test environment.

* Simplifies cross platform extension compilation (targeting Windows from Linux).

## OK, I'm sold! Show me how to install it!

Simple:

    $ gem install rake-compiler

## That's easy. How do I use it?

Now that you have installed rake-compiler, it's time to give your project a
standardized structure.

### Using a standardized project structure

Let's say you want to compile an extension called 'hello_world'. Organizing
your project's code tree in the following way will help rake-compiler do
its job:

    |-- ext
    |   `-- hello_world
    |       |-- extconf.rb
    |       |-- HelloWorldService.java
    |       `-- hello_world.c
    |-- lib
    `-- Rakefile

TIP: Having a consistent project directory structure will help developers and
newcomers find and understand your code, making it easier for them to
contribute back to your project.

### Adding the code to enable rake-compiler

Now the fun part. It's time to introduce the code to your projects Rakefile
to tell it to use rake-compiler to build your extension:

    # File: extconf.rb

    # these lines must exist already
    require 'mkmf'
    create_makefile('hello_world')

    # File: Rakefile

    require 'rake/extensiontask'

    Rake::ExtensionTask.new('hello_world')

That's it? Yes, that's it! No other lines of code are needed for
rake-compiler to work its magic.

Though, you need to make sure the parameter to `create_makefile`
and `ExtensionTask.new` are the same or rake-compiler will not mimic
the RubyGems standard install process. You can override this standard
behaviour if needed, see the instructions for "non-standard project structure"
below for details.

If you want to do the same for a JRuby extension written in Java, it's just
as easy:

    # File: Rakefile

    require 'rake/javaextensiontask'

    Rake::JavaExtensionTask.new('hello_world')

### The simple process

Those **two** simple lines of code automatically added the Rake tasks needed to
build your 'hello_world' extension. For example, checking the Rake tasks on
MRI Ruby 1.8.x/1.9 returns something similar to:

    $ rake -T
    (in /home/user/my_extension)
    rake compile                # Compile the extension(s)
    rake compile:hello_world    # Compile just the hello_world extension

Simply calling `compile` like

    $ rake compile

performs the entire compile and build process for you and places the resulting
extension inside the `lib` directory of your project.

To pass `dir_config` options to the compilation, add to the command line:

    $ rake compile -- --with-foo-[dir|lib|bin|...]=/path/to/foo

NOTE: Please be aware that building C extensions requires the proper
development environment for your Platform, including libraries, headers
and build tools. Check your distro / vendor documentation on how to install
these development resources.

NOTE: Building Java extensions requires the `javac`, part of the Java
Development Kit (JDK). This should be included by default on Mac OS X, and
downloadable from http://java.sun.com for other operating systems.

### Generating native RubyGems

A common usage scenario for rake-compiler is generating native gems that
bundle your extensions. As mentioned above, if you have your development
environment configured correctly, the following examples work even when
building native gems on Windows systems.

Creating native gems is really easy with rake-compiler's
`Rake::ExtensionTask`:

    # somewhere in your Rakefile, define your gem spec
    spec = Gem::Specification.new do |s|
      s.name = "my_gem"
      s.platform = Gem::Platform::RUBY
      s.extensions = FileList["ext/**/extconf.rb"]
    end

    # add your default gem packing task
    Gem::PackageTask.new(spec) do |pkg|
    end

    # feed the ExtensionTask with your spec
    Rake::ExtensionTask.new('hello_world', spec)

As expected, you can still build your pure-ruby gem in the usual way
(standard output) by running:

    $ rake gem
    (in /projects/oss/my_gem.git)
    mkdir -p pkg
      Successfully built RubyGem
      Name: my_gem
      Version: 0.1.0
      File: my_gem-0.1.0.gem
    mv my_gem-0.1.0.gem pkg/my_gem-0.1.0.gem

Plus, rake-compiler tasks give you the extra functionality needed to build
native gems by running:

    # rake native gem
    (... compilation output ...)
    mkdir -p pkg
      Successfully built RubyGem
      Name: my_gem
      Version: 0.1.0
      File: my_gem-0.1.0.gem
    mv my_gem-0.1.0.gem pkg/my_gem-0.1.0.gem
      Successfully built RubyGem
      Name: my_gem
      Version: 0.1.0
      File: my_gem-0.1.0-x86-mingw32.gem
    mv my_gem-0.1.0-x86-mingw32.gem pkg/my_gem-0.1.0-x86-mingw32.gem

Did you notice that you get two gems for the price of one? How's that for a
time saver?

Similarly, it's just as easy to do the same thing for JRuby extensions:

    # rake java gem
    (... compilation output ...)
    mkdir -p pkg
      Successfully built RubyGem
      Name: my_gem
      Version: 0.1.0
      File: my_gem-0.1.0.gem
    mv my_gem-0.1.0.gem pkg/my_gem-0.1.0.gem
      Successfully built RubyGem
      Name: my_gem
      Version: 0.1.0
      File: my_gem-0.1.0-java.gem
    mv my_gem-0.1.0-java.gem pkg/my_gem-0.1.0-java.gem


### Great, but can I use a non-standard project structure?

Yes you can! While the conventional project structure is recommended, you may
want, or need, to tweak those conventions. Rake-compiler allows you to customize
several settings for `Rake::ExtensionTask`:

    Rake::ExtensionTask.new do |ext|
      ext.name = 'hello_world'                # indicate the name of the extension.
      ext.ext_dir = 'ext/weird_world'         # search for 'hello_world' inside it.
      ext.lib_dir = 'lib/my_lib'              # put binaries into this folder.
      ext.config_script = 'custom_extconf.rb' # use instead of the default 'extconf.rb'.
      ext.tmp_dir = 'tmp'                     # temporary folder used during compilation.
      ext.source_pattern = "*.{c,cpp}"        # monitor file changes to allow simple rebuild.
      ext.config_options << '--with-foo'      # supply additional options to configure script.
      ext.gem_spec = spec                     # optionally indicate which gem specification
                                              # will be used.
    end


### Show me all of the supported configuration options

| Option               | Supported By          | Description                              |
| -------------------- | --------------------- | ---------------------------------------- |
| name                 | Both                  | Required. Give the target binary a name. |
| gem_spec             | Both                  | [Optional] Indicate which gem specification will be used. |
| tmp_dir              | Both                  | [Optional] Temporary folder used during compilation. |
| ext_dir              | Both                  | [Optional] Where to search for `name`. Default: `ext/#{@name}`. |
| lib_dir              | Both                  | [Optional] Put binaries into this folder. Default: `lib`. |
| config_options       | Both                  | [Optional] Supply additional options to configure script. |
| source_pattern       | Both                  | [Optional] Monitor file changes to allow simple rebuild. Default for CRuby: `*.{c,cc,cpp}`. Default for Java: `**/*.java`. |
| _extra_options_      | ExtensionTask (CRuby) | [Optional] _Any options you add to ARGV on the command line are passed on as complilation flags if they begin with a dash (-)._ |
| config_script        | ExtensionTask (CRuby) | [Optional] Specify alternate configuration file name when [Adding the code to enable rake-compiler](#adding-the-code-to-enable-rake-compiler). Default: `extconf.rb`.  |
| cross_compile        | ExtensionTask (CRuby) | [Optional] See [Cross compilation - the future is now.](#cross-compilation---the-future-is-now) Default: `false`. |
| cross_platform       | ExtensionTask (CRuby) | [Optional] See [Cross compilation - the future is now.](#cross-compilation---the-future-is-now) Default: `i386-mingw32`. |
| cross_config_options | ExtensionTask (CRuby) | [Optional] See [Cross compilation - the future is now.](#cross-compilation---the-future-is-now) Default: `[]`. |
| no_native            | ExtensionTask (CRuby) | [Optional] Set to true to prevent non-CRuby platforms from defining native tasks. Default: `false`. |
| config_includes      | ExtensionTask (CRuby) | [Optional] Specify an Array of paths to include as `-I...:...` includes during compilation. Default: `['.']`. |
| classpath            | JavaExtensionTask     | [Optional] Specify additional classpath paths as an Array. Default: _Uses the current CLASSPATH._  |
| debug                | JavaExtensionTask     | [Optional] Whether to set the debug flag during complication. Default: `false`. |
| source_version       | JavaExtensionTask     | [Optional] The JRE version that your source code requires to compile. Default: `1.6`. |
| target_version       | JavaExtensionTask     | [Optional] The oldest JRE version you want to support. Default: `1.6`. |
| encoding             | JavaExtensionTask     | [Optional] Specify an -encoding option to provide to the compiler. Default: `nil`. |
| lint_option          | JavaExtensionTask     | [Optional] Specify a `-Xlint:___` linting option such as `deprecation`, `all`, `none`, etc. (Run `javac -help -X` to see all available options.) <br> Default: _Simply `-Xlint` is run, which enables recommended warnings._ |


## Cross compilation - the future is now.

Rake-compiler also provides a standardized way to generate, from either Linux
or OSX, extensions and gem binaries for your Windows users!

How can this be you say? Simple, rake-compiler's cross compilation features
take advantage of GCC's host/target capabilities to build 'target' binaries on
different 'host' OS's.

### How do I do this from Linux or OSX?

#### The Easy Way

Use rake-compiler-dock, a gem that makes use of a virtual machine provisioned with
all the necessary build tools.  You can add a task to your Rakefile, that
cross-compiles and packages your gem into Windows fat binaries (with 1.8 to 2.2
and x86/x64 support). See https://github.com/rake-compiler/rake-compiler-dock for more
information.

#### The Manual Way

In addition to having the development tool chain installed (GCC), you also need to
install your platform's `mingw32` cross compilation package.

Installation depends upon your operating system/distribution. On Ubuntu and Debian
host machines, a simple `apt-get install mingw32` will be enough.

On Arch, `mingw32` is installed by running `pacman -S mingw32-gcc`

On OSX, we no longer recommend the usage of MacPorts `mingw32` package because
it stagnated in GCC version 3.4.5.

Instead we recommend you download mingw-w64 automated build packages available at
SourceForge:

http://sourceforge.net/downloads/mingw-w64/

Browse into *Toolchains targetting Win32* and then *Automated Builds*.

Files will be ordered by recency, find the latest one with version 1.0 in it,
like this one:

    mingw-w32-1.0-bin_i686-darwin_20110422.tar.bz2

Download and extract. After that, make sure the bin directory is added to the PATH, eg:

    export PATH=~/mingw-w64/w32/bin:$PATH

You can add this to your `.profile` to avoid the repitition.

#### I've got my tool-chain installed, now what?

First, you need to build Ruby for Windows on your Linux or OSX system.

Relax, no need to freak out! Let rake-compiler do all the heavy lifting for you:

    rake-compiler cross-ruby

And you're done. It will automatically download, configure and compile the latest
stable version of Ruby for Windows, and place it into your `~/.rake-compiler`
directory.

This will create `~/.rake-compiler/config.yml` file so that rake-compiler
knows where to find the `rbconfig.rb` file that matches the Ruby version
on the Windows host system you're cross-compiling for. An example:

    # File: ~/.rake-compiler/config.yml

    rbconfig-x86-mingw32-1.8.6: /path/to/ruby-1.8.6/rbconfig.rb
    rbconfig-x86-mingw32-1.8.7: /path/to/ruby-1.8.7/rbconfig.rb
    rbconfig-x86-mingw32-1.9.2: /path/to/ruby-1.9.2/rbconfig.rb

If, instead, you want to build a different Ruby version than the default one, please
supply a `VERSION`:

    rake-compiler cross-ruby VERSION=1.8.6-p114

If you, like me, have multiple versions of MinGW packages installed, you can
specify the HOST that will be used to cross compile Ruby:

    rake-compiler cross-ruby HOST=x86-mingw32 # (OSX mingw32 port)

The host will vary depending on provider (mingw32 versus mingw-w64 projects).
Please consult the documentation and website of the MinGW package provider before
reporting any issues.

#### OK, let's cross compile some gems!

Now, you only need specify a few additional options in your extension definition:

    Rake::ExtensionTask.new('my_extension', gem_spec) do |ext|
      # enable cross compilation (requires cross compile toolchain)
      ext.cross_compile = true

      # set a single platform or an array of platforms to target
      ext.cross_platform = ['x86-mingw32', 'x64-mingw32']

      # cross-compile options will be passed to extconf.rb for each
      # platform build, with platform-specific options in a hash.
      ext.cross_config_options << '--with-common-option'
      ext.cross_config_options << {
        'x86-mswin32-60 => '--with-some-option',
        'x64-mingw32'   => '--enable-64bits',
      }
      ext.cross_config_options << '--with-final-option'

      # perform alterations on the gemspec when cross compiling
      ext.cross_compiling do |gem_spec|
        # such as packaging a file that isn't specified in the gemspec
        gem_spec.files << 'lib/generated_file.rb'
        # or adding a new installation message
        gem_spec.post_install_message = "You installed the binary version of this gem!"
      end
    end

By default, cross compilation targets 'i386-mingw32' which is the default
GCC platform for Ruby. MRI Ruby's current official distribution uses
`i386-mswin32-60`. The RubyInstaller distribution uses
`x86-mingw32` and `x64-mingw32` for 32-bit and 64-bit
Windows targets, respectively. Note that `i386` and `x86`
are synonymous here; `x86` is preferred going forward.

The format for `cross_config_options` is an array of strings and
hashes. Hashes will be fetched for each value of `cross_platform`
as the build iterates, or ignored if there is no value for that platform.
You can mix-and-match strings and hashes to get desired option ordering.

#### Warning, magician about to do some tricks, don't blink!

Cross compiling is still very simple:

    rake cross compile

And now, building gems for your Windows users is just 6 more letters:

    rake cross native gem

And you're done, yeah.

#### But wait, there's more

You can specify which version of Ruby to build the extension against:

    rake cross compile RUBY_CC_VERSION=1.8.6

For example, if you installed `1.9.2`, you can do:

    rake cross compile RUBY_CC_VERSION=1.9.2

Even better, you can target multiple versions (ie. 1.8.6 and 1.9.2) in
the same gem via:

    rake cross compile RUBY_CC_VERSION=1.8.6:1.9.2

And better yet, you can bundle both binary extensions into one so-called "fat"
gem via:

    rake cross native gem RUBY_CC_VERSION=1.8.6:1.9.2

That will place binaries for both the 1.8 and 1.9 versions of your Ruby
extensions inside your project's `lib_dir` directory:

    lib/1.8/my_extension.so
    lib/1.9/my_extension.so

NOTE: building "fat" gems is currently only supported by rake-compiler when
cross compiling from a Linux or OSX host. Patches are welcome if building
"fat" gems from Windows hosts is desired, or natively for your platform :-)

Now it's up to you to make your gem load the proper binary at runtime:

    begin
      RUBY_VERSION =~ /(\d+\.\d+)/
      require "#{$1}/my_extension"
    rescue LoadError
      require "my_extension"
    end

The above technique will lookup first for 1.8 or 1.9 version of the extension
and when not found, will look for the plain extension.

This approach catch the cases of provided fat binaries or gems compiled by the
end user installing the gem. It has also been implemented successfully in
several projects.

## What are you talking about? (Give me examples)

I know all the above sounds like a complete foreign language (it does even for me!).
So, what if I show you some examples?

Check our wiki with links to the proper rake files used by many developers and
projects and how they use rake-compiler.

http://github.com/rake-compiler/rake-compiler/wiki/projects-using-rake-compiler

## Future

rake-compiler is a work in progress and we appreciate any and all feedback
during the development of it! (and contributions too!)

You can find more information about rake-compiler:

*   GitHub:    https://github.com/rake-compiler/rake-compiler
*   Issues:    https://github.com/rake-compiler/rake-compiler/issues
*   Docs:      http://rubydoc.info/gems/rake-compiler
*   Wiki:      https://github.com/rake-compiler/rake-compiler/wiki

## Disclaimer

If you have any trouble, don't hesitate to contact the author. As always,
I'm not going to say "Use at your own risk" because I don't want this library
to be risky.

If you trip on something, I'll share the liability by repairing things
as quickly as I can. Your responsibility is to report the inadequacies.
