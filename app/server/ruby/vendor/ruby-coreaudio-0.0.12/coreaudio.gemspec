# -*- encoding: utf-8 -*-
require File.expand_path('../lib/coreaudio/version', __FILE__)

Gem::Specification.new do |gem|
  gem.required_rubygems_version = Gem::Requirement.new(">= 0") if gem.respond_to? :required_rubygems_version=

  gem.authors       = ["CHIKANAGA Tomoyuki"]
  gem.email         = ["nagachika00@gmail.com"]
  gem.description   = "Mac OS X CoreAudio wrapper library"
  gem.summary       = "Mac OS X CoreAudio wrapper library"
  gem.homepage      = "https://github.com/nagachika/ruby-coreaudio"

  gem.executables   = `git ls-files -- bin/*`.split("\n").map{ |f| File.basename(f) }
  gem.files         = `git ls-files`.split("\n")
  gem.test_files    = `git ls-files -- {test,spec,features}/*`.split("\n")
  gem.name          = "coreaudio"
  gem.require_paths = ["lib"]
  gem.version       = CoreAudio::VERSION

  gem.extensions = ["ext/coreaudio/extconf.rb"]
  gem.extra_rdoc_files = [
    "ChangeLog",
    "LICENSE.txt",
    "README.rdoc"
  ]
  gem.licenses = ["BSDL"]

  if gem.respond_to? :specification_version then
    gem.specification_version = 3

    if Gem::Version.new(Gem::VERSION) >= Gem::Version.new('1.2.0') then
      gem.add_runtime_dependency(%q<narray>, ["~> 0.6.0.0"])
      gem.add_development_dependency(%q<bundler>, [">= 0"])
      gem.add_development_dependency(%q<rake>, [">= 0"])
      gem.add_development_dependency(%q<rdoc>, [">= 0"])
    else
      gem.add_dependency(%q<narray>, ["~> 0.6.0.0"])
      gem.add_dependency(%q<bundler>, [">= 0"])
      gem.add_dependency(%q<rake>, [">= 0"])
      gem.add_dependency(%q<rdoc>, [">= 0"])
    end
  else
    gem.add_dependency(%q<narray>, ["~> 0.6.0.0"])
    gem.add_dependency(%q<bundler>, [">= 0"])
    gem.add_dependency(%q<rake>, [">= 0"])
    gem.add_dependency(%q<rdoc>, [">= 0"])
  end
end

