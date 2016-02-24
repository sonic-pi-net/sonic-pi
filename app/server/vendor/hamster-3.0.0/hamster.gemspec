# coding: utf-8
lib = File.expand_path("../lib", __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require "hamster/version"

Gem::Specification.new do |spec|
  spec.name          = "hamster"
  spec.version       = Hamster::VERSION
  spec.authors       = ["Simon Harris"]
  spec.email         = ["haruki_zaemon@mac.com"]
  spec.summary       = %q{Efficient, immutable, thread-safe collection classes for Ruby}
  spec.description   = spec.summary
  spec.homepage      = "https://github.com/hamstergem/hamster"
  spec.license       = "MIT"

  spec.platform      = Gem::Platform::RUBY
  spec.required_ruby_version = ">= 1.9.3"

  spec.files         = Dir["lib/**/*", "LICENSE"]
  spec.executables   = Dir["bin/**/*"].map! { |f| f.gsub(/bin\//, '') }
  spec.test_files    = Dir["test/**/*", "spec/**/*"]
  spec.require_paths = ["lib"]

  spec.add_runtime_dependency     "concurrent-ruby", "~> 1.0"
  spec.add_development_dependency "bundler", "~> 1.3"
  spec.add_development_dependency "rspec", "~> 3.0"
  spec.add_development_dependency "rake", "~> 10.1"
  spec.add_development_dependency "yard", "~> 0.8"
  spec.add_development_dependency "pry", "~> 0.9"
  spec.add_development_dependency "pry-doc", "~> 0.6"
  spec.add_development_dependency "benchmark-ips", "~> 2.1"
  spec.add_development_dependency "codeclimate-test-reporter", "~> 0.4"
end
