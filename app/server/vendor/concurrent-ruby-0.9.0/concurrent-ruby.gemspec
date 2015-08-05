$:.push File.join(File.dirname(__FILE__), 'lib')
$:.push File.join(File.dirname(__FILE__), 'support')

require 'concurrent/version'
require 'file_map'

Gem::Specification.new do |s|
  git_files = `git ls-files`.split("\n")

  s.name             = 'concurrent-ruby'
  s.version          = Concurrent::VERSION
  s.platform         = Gem::Platform::RUBY
  s.authors          = ["Jerry D'Antonio", 'The Ruby Concurrency Team']
  s.email            = ['jerry.dantonio@gmail.com', 'concurrent-ruby@googlegroups.com']
  s.homepage         = 'http://www.concurrent-ruby.com'
  s.summary          = 'Modern concurrency tools for Ruby. Inspired by Erlang, Clojure, Scala, Haskell, F#, C#, Java, and classic concurrency patterns.'
  s.license          = 'MIT'
  s.date             = Time.now.strftime('%Y-%m-%d')
  s.files            = FileMap::MAP.fetch(:core)
  s.extra_rdoc_files = Dir['README*', 'LICENSE*', 'CHANGELOG*']
  s.require_paths    = ['lib']
  s.description      = <<-EOF
Modern concurrency tools including agents, futures, promises, thread pools, actors, supervisors, and more.
Inspired by Erlang, Clojure, Go, JavaScript, actors, and classic concurrency patterns.
  EOF

  if defined?(JRUBY_VERSION)
    s.files    += Dir['lib/**/*.jar']
    s.platform = 'java'
  end

  s.required_ruby_version = '>= 1.9.3'
end
