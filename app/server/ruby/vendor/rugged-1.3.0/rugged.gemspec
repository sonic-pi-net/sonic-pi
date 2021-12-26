$:.push File.expand_path("../lib", __FILE__)

if ENV['DEVELOPMENT']
  VERSION = `git describe --tags`.strip.gsub('-', '.')[1..-1]
else
  require 'rugged/version'
  VERSION = Rugged::Version
end

Gem::Specification.new do |s|
  s.name                  = "rugged"
  s.version               = VERSION
  s.date                  = Time.now.strftime('%Y-%m-%d')
  s.summary               = "Rugged is a Ruby binding to the libgit2 linkable library"
  s.homepage              = "https://github.com/libgit2/rugged"
  s.email                 = "schacon@gmail.com"
  s.authors               = [ "Scott Chacon", "Vicent Marti" ]
  s.license               = "MIT"
  s.files                 = %w( README.md LICENSE )
  s.files                 += Dir.glob("lib/**/*.rb")
  s.files                 += Dir.glob("ext/**/*.[ch]")
  s.files                 += Dir.glob("vendor/libgit2/cmake/**/*")
  s.files                 += Dir.glob("vendor/libgit2/{include,src,deps}/**/*")
  s.files                 += Dir.glob("vendor/libgit2/{CMakeLists.txt,Makefile.embed,AUTHORS,COPYING,libgit2.pc.in}")
  s.extensions            = ['ext/rugged/extconf.rb']
  s.required_ruby_version = '>= 1.9.3'
  s.description           = <<desc
Rugged is a Ruby bindings to the libgit2 linkable C Git library. This is
for testing and using the libgit2 library in a language that is awesome.
desc
  s.add_development_dependency "rake-compiler", ">= 0.9.0"
  s.add_development_dependency "pry"
  s.add_development_dependency "minitest", "~> 5.0"
  s.metadata["msys2_mingw_dependencies"] = "libssh2 cmake"
end
