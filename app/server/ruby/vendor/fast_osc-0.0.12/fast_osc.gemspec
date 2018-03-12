# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'fast_osc/version'

Gem::Specification.new do |spec|
  spec.name          = "fast_osc"
  spec.version       = FastOsc::VERSION
  spec.authors       = ["Xavier Riley"]
  spec.email         = ["xavriley@hotmail.com"]
  spec.summary       = %q{Serialize and deserialize Open Sound Control messages}
  spec.description   = %q{Serialize and deserialize Open Sound Control messages using rtosc}
  spec.homepage      = "https://github.com/xavriley/fast_osc"
  spec.license       = "MIT"

  spec.files         = `git ls-files -z`.split("\x0")
  spec.executables   = spec.files.grep(%r{^bin/}) { |f| File.basename(f) }
  spec.test_files    = spec.files.grep(%r{^(test|spec|features)/})
  spec.require_paths = ["lib", "ext"]

  spec.extensions << "ext/fast_osc/extconf.rb"

  spec.add_development_dependency "bundler", "~> 1.5"
  spec.add_development_dependency "rake"
  spec.add_development_dependency "rake-compiler"
  spec.add_development_dependency "minitest", "~> 5.0"
	spec.add_development_dependency "osc-ruby", "~> 1.1.1"
	spec.add_development_dependency "benchmark-ips", "~> 2.7.2"
end
