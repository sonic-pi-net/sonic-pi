# -*- encoding: utf-8 -*-
$:.push File.expand_path("../lib", __FILE__)
require "metaclass/version"

Gem::Specification.new do |s|
  s.name        = "metaclass"
  s.version     = Metaclass::VERSION
  s.authors     = ["James Mead"]
  s.email       = ["james@floehopper.org"]
  s.homepage    = "http://github.com/floehopper/metaclass"
  s.summary     = %q{Adds a metaclass method to all Ruby objects}
  s.license     = "MIT"

  s.rubyforge_project = "metaclass"

  s.files         = `git ls-files`.split("\n")
  s.test_files    = `git ls-files -- {test,spec,features}/*`.split("\n")
  s.executables   = `git ls-files -- bin/*`.split("\n").map{ |f| File.basename(f) }
  s.require_paths = ["lib"]
end