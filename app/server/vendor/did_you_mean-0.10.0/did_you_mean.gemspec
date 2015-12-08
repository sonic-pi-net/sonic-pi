# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'did_you_mean/version'

Gem::Specification.new do |spec|
  spec.name          = "did_you_mean"
  spec.version       = DidYouMean::VERSION
  spec.authors       = ["Yuki Nishijima"]
  spec.email         = ["mail@yukinishijima.net"]
  spec.summary       = '"Did you mean?" experience in Ruby'
  spec.description   = '"did you mean?" experience in Ruby: the error message will tell you the right one when you misspelled something.'
  spec.homepage      = "https://github.com/yuki24/did_you_mean"
  spec.license       = "MIT"

  spec.files         = `git ls-files`.split($/)
  spec.executables   = spec.files.grep(%r{^bin/}) { |f| File.basename(f) }
  spec.test_files    = spec.files.grep(%r{^(test|spec|features)/})
  spec.require_paths = ["lib"]

  spec.required_ruby_version = '>= 1.9.3'

  case RUBY_ENGINE
  when 'ruby'
    spec.extensions = ["ext/did_you_mean/extconf.rb"]
  when 'jruby'
    spec.platform = 'java'
  end

  spec.add_dependency "interception"

  spec.add_development_dependency "bundler", "~> 1.5"
  spec.add_development_dependency "rake"
  spec.add_development_dependency "rake-compiler"
  spec.add_development_dependency "minitest"
end
