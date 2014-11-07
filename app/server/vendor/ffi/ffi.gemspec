require File.expand_path("../lib/#{File.basename(__FILE__, '.gemspec')}/version", __FILE__)

Gem::Specification.new do |s|
  s.name = 'ffi'
  s.version = FFI::VERSION
  s.author = 'Wayne Meissner'
  s.email = 'wmeissner@gmail.com'
  s.homepage = 'http://wiki.github.com/ffi/ffi'
  s.summary = 'Ruby FFI'
  s.description = 'Ruby FFI library'
  s.files = %w(ffi.gemspec LICENSE COPYING README.md Rakefile) + Dir.glob("{ext,gen,lib,spec,libtest}/**/*").reject { |f| f =~ /(lib\/[12]\.[089]|\.so$|\.bundle|\.dylib$)/ }
  s.extensions << 'ext/ffi_c/extconf.rb'
  s.has_rdoc = false
  s.rdoc_options = %w[--exclude=ext/ffi_c/.*\.o$ --exclude=ffi_c\.(bundle|so)$]
  s.license = 'BSD'
  s.require_paths << 'ext/ffi_c'
  s.required_ruby_version = '>= 1.8.7'
  s.add_development_dependency 'rake', '~> 10.1'
  s.add_development_dependency 'rake-compiler', '~> 0.9'
  s.add_development_dependency 'rspec', '~> 2.14.1'
  s.add_development_dependency 'rubygems-tasks', "~> 0.2.4"
end
