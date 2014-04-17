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
  spec.homepage      = "http://harukizaemon.github.io/hamster"
  spec.license       = "MIT"

  spec.platform      = Gem::Platform::RUBY
  spec.required_ruby_version = ">= 1.8.7"

  spec.files         = Dir["lib/**/*"]
  spec.executables   = Dir["bin/**/*"].map! { |f| f.gsub(/bin\//, '') }
  spec.test_files    = Dir["test/**/*", "spec/**/*"]
  spec.require_paths = ["lib"]

  spec.add_development_dependency "bundler", "~> 1.3"
  spec.add_development_dependency "rspec", "~> 2.14"
  spec.add_development_dependency "mocha", "~> 0.14"
  spec.add_development_dependency "rake", "~> 10.1"
  spec.add_development_dependency "yard", "~> 0.8"
  spec.add_development_dependency "kramdown", "~> 1.2"
  spec.add_development_dependency "pry", "~> 0.9"
  spec.add_development_dependency "coveralls", "~> 0.7"
  spec.add_development_dependency "rubocop", "~> 0.15"
end
