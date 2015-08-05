$:.push File.join(File.dirname(__FILE__), 'lib')
$:.push File.join(File.dirname(__FILE__), 'support')

require 'concurrent/version'
require 'file_map'

Gem::Specification.new do |s|
  git_files = `git ls-files`.split("\n")

  s.name             = 'concurrent-ruby-edge'
  s.version          = Concurrent::EDGE_VERSION
  s.platform         = Gem::Platform::RUBY
  s.authors          = ["Jerry D'Antonio", 'Petr Chalupa', 'The Ruby Concurrency Team']
  s.email            = ['jerry.dantonio@gmail.com', 'concurrent-ruby@googlegroups.com']
  s.homepage         = 'http://www.concurrent-ruby.com'
  s.summary          = 'Edge features and additions to the concurrent-ruby gem.'
  s.license          = 'MIT'
  s.date             = Time.now.strftime('%Y-%m-%d')
  s.files            = FileMap::MAP.fetch(:edge)
  s.extra_rdoc_files = Dir['README*', 'LICENSE*']
  s.require_paths    = ['lib']
  s.description      = <<-TXT
These features are under active development and may change frequently. They are expected not to
keep backward compatibility (there may also lack tests and documentation). Semantic versions will
be obeyed though. Features developed in `concurrent-ruby-edge` are expected to move to `concurrent-ruby` when final.
Please see http://concurrent-ruby.com for more information.
  TXT

  s.required_ruby_version = '>= 1.9.3'

  s.add_runtime_dependency 'concurrent-ruby', "~> #{Concurrent::VERSION}"
end
