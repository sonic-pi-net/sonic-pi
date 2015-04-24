# encoding: utf-8

require File.expand_path('../lib/parser/version', __FILE__)

Gem::Specification.new do |spec|
  spec.name          = 'parser'
  spec.version       = Parser::VERSION
  spec.authors       = ['Peter Zotov']
  spec.email         = ['whitequark@whitequark.org']
  spec.description   = 'A Ruby parser written in pure Ruby.'
  spec.summary       = spec.description
  spec.homepage      = 'https://github.com/whitequark/parser'
  spec.license       = 'MIT'
  spec.has_rdoc      = 'yard'

  spec.files         = `git ls-files`.split + %w(
                          lib/parser/lexer.rb
                          lib/parser/ruby18.rb
                          lib/parser/ruby19.rb
                          lib/parser/ruby20.rb
                          lib/parser/ruby21.rb
                          lib/parser/ruby22.rb
                       )
  spec.executables   = spec.files.grep(%r{^bin/}) { |f| File.basename(f) }
  spec.test_files    = spec.files.grep(%r{^test/})
  spec.require_paths = ['lib']

  spec.add_dependency             'ast',       ['>= 1.1', '< 3.0']

  spec.add_development_dependency 'bundler',   '~> 1.2'
  spec.add_development_dependency 'rake',      '~> 10.0'
  spec.add_development_dependency 'racc',      '= 1.4.9' # update to 1.4.11 when it's done
  spec.add_development_dependency 'cliver',    '~> 0.3.0'

  spec.add_development_dependency 'yard'
  spec.add_development_dependency 'kramdown'

  spec.add_development_dependency 'minitest',  '~> 5.0'
  spec.add_development_dependency 'simplecov', '~> 0.8.2'
  spec.add_development_dependency 'coveralls'
  spec.add_development_dependency 'json_pure' # for coveralls on 1.9.2
  spec.add_development_dependency 'mime-types', '~> 1.25' # for coveralls on 1.8.7
  spec.add_development_dependency 'rest-client', '~> 1.6.7' # 1.8.7

  spec.add_development_dependency 'simplecov-sublime-ruby-coverage'

  spec.add_development_dependency 'gauntlet'
end
