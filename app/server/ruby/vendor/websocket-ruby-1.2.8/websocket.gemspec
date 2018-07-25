# frozen_string_literal: true

$LOAD_PATH.push File.expand_path('../lib', __FILE__)
require 'websocket/version'

Gem::Specification.new do |s|
  s.name        = 'websocket'
  s.version     = WebSocket::VERSION
  s.platform    = Gem::Platform::RUBY
  s.authors     = ['Bernard Potocki']
  s.email       = ['bernard.potocki@imanel.org']
  s.homepage    = 'http://github.com/imanel/websocket-ruby'
  s.summary     = 'Universal Ruby library to handle WebSocket protocol'
  s.description = 'Universal Ruby library to handle WebSocket protocol'
  s.license     = 'MIT'

  s.files         = `git ls-files`.split("\n")
  s.test_files    = `git ls-files -- {test,spec,features}/*`.split("\n")
  s.executables   = `git ls-files -- bin/*`.split("\n").map { |f| File.basename(f) }
  s.require_paths = ['lib']

  s.required_ruby_version = '>= 2.0'
end
