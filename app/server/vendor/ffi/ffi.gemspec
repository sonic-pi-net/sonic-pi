require File.expand_path("../lib/#{File.basename(__FILE__, '.gemspec')}/version", __FILE__)

Gem::Specification.new do |s|
  s.name = 'ffi'
  s.version = FFI::VERSION
  s.author = 'Wayne Meissner'
  s.email = 'wmeissner@gmail.com'
  s.homepage = 'http://wiki.github.com/ffi/ffi'
  s.summary = 'Ruby FFI'
  s.description = 'Ruby FFI library'
  s.files = %w(ffi.gemspec LICENSE COPYING README.md Rakefile) + Dir.glob("{ext,gen,lib,spec,libtest}/**/*").reject { |f| f =~ /(lib\/[12]\.[089]|\.so$|\.bundle$)/ }
  s.extensions << 'ext/ffi_c/extconf.rb'
  s.has_rdoc = false
  s.rdoc_options = %w[--exclude=ext/ffi_c/.*\.o$ --exclude=ffi_c\.(bundle|so)$]
  s.license = 'BSD'
  s.require_paths << 'ext/ffi_c'
  s.required_ruby_version = '>= 1.8.7'
  s.add_development_dependency 'rake'
  s.add_development_dependency 'rake-compiler', '>=0.6.0'
  s.add_development_dependency 'rspec'
  s.add_development_dependency 'rubygems-tasks'
end
