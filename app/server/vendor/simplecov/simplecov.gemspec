$LOAD_PATH.push File.expand_path("../lib", __FILE__)
require "simplecov/version"

Gem::Specification.new do |gem|
  gem.name        = "simplecov"
  gem.version     = SimpleCov::VERSION
  gem.platform    = Gem::Platform::RUBY
  gem.authors     = ["Christoph Olszowka"]
  gem.email       = ["christoph at olszowka de"]
  gem.homepage    = "http://github.com/colszowka/simplecov"
  gem.description = %(Code coverage for Ruby 1.9+ with a powerful configuration library and automatic merging of coverage across test suites)
  gem.summary     = gem.description
  gem.license     = "MIT"

  gem.required_ruby_version = ">= 1.8.7"

  gem.add_dependency "json", ">= 1.8", "< 3"
  gem.add_dependency "simplecov-html", "~> 0.10.0"
  gem.add_dependency "docile", "~> 1.1.0"

  gem.add_development_dependency "bundler", "~> 1.9"

  gem.files         = `git ls-files`.split("\n")
  gem.test_files    = `git ls-files -- {test,spec,features}/*`.split("\n")
  gem.executables   = `git ls-files -- bin/*`.split("\n").map { |f| File.basename(f) }
  gem.require_paths = ["lib"]
end
