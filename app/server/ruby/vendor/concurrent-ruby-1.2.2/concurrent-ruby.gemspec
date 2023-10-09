require File.join(File.dirname(__FILE__ ), 'lib/concurrent-ruby/concurrent/version')

Gem::Specification.new do |s|
  git_files = `git ls-files`.split("\n")

  s.name             = 'concurrent-ruby'
  s.version          = Concurrent::VERSION
  s.platform         = Gem::Platform::RUBY
  s.authors          = ["Jerry D'Antonio", 'Petr Chalupa', 'The Ruby Concurrency Team']
  s.email            = 'concurrent-ruby@googlegroups.com'
  s.homepage         = 'http://www.concurrent-ruby.com'
  s.summary          = 'Modern concurrency tools for Ruby. Inspired by Erlang, Clojure, Scala, Haskell, F#, C#, Java, and classic concurrency patterns.'
  s.license          = 'MIT'
  s.date             = Time.now.strftime('%Y-%m-%d')
  s.files            = [*Dir['lib/concurrent-ruby/**/*.rb'] & git_files,
                        *Dir['ext/concurrent-ruby/**/*'] & git_files,
                        'Rakefile',
                        'Gemfile',
                        'lib/concurrent-ruby/concurrent/concurrent_ruby.jar'
  ]
  s.extra_rdoc_files = Dir['README*', 'LICENSE*', 'CHANGELOG*']
  s.require_paths    = ['lib/concurrent-ruby']
  s.description      = <<-TXT.gsub(/^ +/, '')
    Modern concurrency tools including agents, futures, promises, thread pools, actors, supervisors, and more.
    Inspired by Erlang, Clojure, Go, JavaScript, actors, and classic concurrency patterns.
  TXT
  s.metadata["source_code_uri"] = "https://github.com/ruby-concurrency/concurrent-ruby"
  s.metadata["changelog_uri"] = "https://github.com/ruby-concurrency/concurrent-ruby/blob/master/CHANGELOG.md"
  s.required_ruby_version = '>= 2.3'
end
