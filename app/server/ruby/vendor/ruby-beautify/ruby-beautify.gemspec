require File.expand_path('../lib/ruby-beautify/version', __FILE__)

Gem::Specification.new do |gem|
  gem.name        = 'ruby-beautify'
  gem.summary     = "a cli tool (and module) to beautify ruby code."
  gem.description = gem.summary
  gem.authors     = ["Ernie Brodeur", "Craig Williams", "Joel Chippindale", "Paul Lutus"]
  gem.email       = 'ebrodeur@ujami.net'
  gem.homepage    = "https://github.com/erniebrodeur/ruby-beautify"

  gem.add_development_dependency "rspec"
  gem.files         = `git ls-files`.split("\n")
  gem.test_files    = `git ls-files -- {test,spec,features}/*`.split("\n")
  gem.executables   = `git ls-files -- bin/*`.split("\n").map{ |f| File.basename(f) }
  gem.require_paths = ["lib"]
  gem.version       = RBeautify::VERSION
end
