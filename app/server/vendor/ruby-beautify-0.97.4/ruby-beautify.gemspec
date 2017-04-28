require File.expand_path('../lib/ruby-beautify/version', __FILE__)

Gem::Specification.new do |gem|
  gem.required_ruby_version = '>= 2.0'
  gem.name        = 'ruby-beautify'
  gem.summary     = "a cli tool (and module) to beautify ruby code."
  gem.description = gem.summary
  gem.authors     = ["Ernie Brodeur"]
  gem.email       = 'ebrodeur@ujami.net'
  gem.homepage    = "https://github.com/erniebrodeur/ruby-beautify"

  gem.files         = `git ls-files`.split("\n")
  gem.test_files    = `git ls-files -- {test,spec,features}/*`.split("\n")
  gem.executables   = `git ls-files -- bin/*`.split("\n").map{ |f| File.basename(f) }
  gem.require_paths = ["lib"]
  gem.version       = RubyBeautify::VERSION
  gem.add_development_dependency 'rake'
  gem.add_development_dependency 'bundler'
  gem.add_development_dependency 'rspec'
end
