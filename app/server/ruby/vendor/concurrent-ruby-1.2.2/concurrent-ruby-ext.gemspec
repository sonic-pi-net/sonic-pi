require File.join(File.dirname(__FILE__ ), 'lib/concurrent-ruby/concurrent/version')

Gem::Specification.new do |s|
  s.name        = 'concurrent-ruby-ext'
  s.version     = Concurrent::VERSION
  s.platform    = Gem::Platform::RUBY
  s.authors     = ["Jerry D'Antonio", 'The Ruby Concurrency Team']
  s.email       = 'concurrent-ruby@googlegroups.com'
  s.homepage    = 'http://www.concurrent-ruby.com'
  s.summary     = 'C extensions to optimize concurrent-ruby under MRI.'
  s.license     = 'MIT'
  s.date        = Time.now.strftime('%Y-%m-%d')

  s.description = <<-EOF
    C extensions to optimize the concurrent-ruby gem when running under MRI.
    Please see http://concurrent-ruby.com for more information.
  EOF

  s.files            = Dir['ext/**/*.{h,c,cpp}']
  s.extra_rdoc_files = Dir['README*', 'LICENSE*', 'CHANGELOG*']
  s.require_paths    = ['lib']
  s.extensions       = 'ext/concurrent-ruby-ext/extconf.rb'

  s.required_ruby_version = '>= 2.3'

  s.add_runtime_dependency 'concurrent-ruby', "= #{Concurrent::VERSION}"
end
