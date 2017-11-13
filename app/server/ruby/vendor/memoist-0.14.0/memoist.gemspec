# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'memoist/version'

AUTHORS = [
  ["Joshua Peek",               "josh@joshpeek.com"],
  ["Tarmo Tänav",               "tarmo@itech.ee"],
  ["Jeremy Kemper",             "jeremy@bitsweat.net"],
  ["Eugene Pimenov",            "libc@mac.com"],
  ["Xavier Noria",              "fxn@hashref.com"],
  ["Niels Ganser",              "niels@herimedia.co"],
  ["Carl Lerche & Yehuda Katz", "wycats@gmail.com"],
  ["jeem",                      "jeem@hughesorama.com"],
  ["Jay Pignata",               "john.pignata@gmail.com"],
  ["Damien Mathieu",            "42@dmathieu.com"],
  ["José Valim",                "jose.valim@gmail.com"],
  ["Matthew Rudy Jacobs",       "matthewrudyjacobs@gmail.com"],
]

Gem::Specification.new do |spec|
  spec.name          = "memoist"
  spec.version       = Memoist::VERSION
  spec.authors       = AUTHORS.map{ |name, email| name }
  spec.email         = AUTHORS.map{ |name, email| email }
  spec.summary       = %q{memoize methods invocation}
  spec.homepage      = "https://github.com/matthewrudy/memoist"
  spec.license       = "MIT"

  spec.files         = `git ls-files -z`.split("\x0")
  spec.executables   = spec.files.grep(%r{^bin/}) { |f| File.basename(f) }
  spec.test_files    = spec.files.grep(%r{^(test|spec|features)/})
  spec.require_paths = ["lib"]

  spec.add_development_dependency "bundler"
  spec.add_development_dependency "rake"
  spec.add_development_dependency "minitest", "~> 5.5.1"
end
