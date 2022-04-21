# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'tomlrb/version'

Gem::Specification.new do |spec|
  spec.name          = "tomlrb"
  spec.version       = Tomlrb::VERSION
  spec.authors       = ["Francois Bernier"]
  spec.email         = ["frankbernier@gmail.com"]

  spec.summary       = %q{A racc based toml parser}
  spec.description   = %q{A racc based toml parser}
  spec.homepage      = "https://github.com/fbernier/tomlrb"
  spec.license       = "MIT"

  spec.files         = %w{LICENSE.txt} + Dir.glob("lib/**/*", File::FNM_DOTMATCH).reject { |f| File.directory?(f) }
  spec.require_paths = ["lib"]
  spec.required_ruby_version = '>= 2.0'
end
