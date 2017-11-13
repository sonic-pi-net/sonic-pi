# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'multi_json/version'

Gem::Specification.new do |spec|
  spec.add_development_dependency 'bundler', '~> 1.0'
  spec.authors       = ["Michael Bleigh", "Josh Kalderimis", "Erik Michaels-Ober", "Pavel Pravosud"]
  spec.cert_chain    = %w(certs/rwz.pem)
  spec.description   = %q{A common interface to multiple JSON libraries, including Oj, Yajl, the JSON gem (with C-extensions), the pure-Ruby JSON gem, NSJSONSerialization, gson.rb, JrJackson, and OkJson.}
  spec.email         = ['michael@intridea.com', 'josh.kalderimis@gmail.com', 'sferik@gmail.com']
  spec.files         = Dir['.yardopts', 'CHANGELOG.md', 'CONTRIBUTING.md', 'LICENSE.md', 'README.md', 'Rakefile', 'multi_json.gemspec', 'Gemfile', '.document', '.rspec', '.travis.yml' ,'spec/**/*', 'lib/**/*']
  spec.homepage      = 'http://github.com/intridea/multi_json'
  spec.licenses      = ['MIT']
  spec.name          = 'multi_json'
  spec.require_paths = ['lib']
  spec.required_rubygems_version = '>= 1.3.5'
  spec.signing_key   = File.expand_path("~/.gem/private_key.pem") if $0 =~ /gem\z/
  spec.summary       = %q{A common interface to multiple JSON libraries.}
  spec.test_files    = Dir['spec/**/*']
  spec.version       = MultiJson::Version
end
