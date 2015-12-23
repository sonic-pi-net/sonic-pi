Gem::Specification.new do |s|
  s.name        = 'ast'
  s.version     = '2.0.0'
  s.license     = 'MIT'
  s.authors     = ["Peter Zotov"]
  s.email       = ["whitequark@whitequark.org"]
  s.homepage    = "https://whitequark.github.io/ast/"
  s.summary     = %q{A library for working with Abstract Syntax Trees.}
  s.description = s.summary

  s.files         = `git ls-files`.split("\n")
  s.test_files    = `git ls-files -- {test,spec,features}/*`.split("\n")
  s.executables   = `git ls-files -- bin/*`.split("\n").map{ |f| File.basename(f) }
  s.require_paths = ["lib"]

  s.add_development_dependency 'rake',                '~> 10.0'

  s.add_development_dependency 'bacon',               '~> 1.2'
  s.add_development_dependency 'bacon-colored_output'
  s.add_development_dependency 'simplecov'

  s.add_development_dependency 'coveralls'
  s.add_development_dependency 'json_pure' # for coveralls on 1.9.2
  s.add_development_dependency 'mime-types', '~> 1.25' # for coveralls on 1.8.7

  s.add_development_dependency 'yard'
  s.add_development_dependency 'kramdown'
end
