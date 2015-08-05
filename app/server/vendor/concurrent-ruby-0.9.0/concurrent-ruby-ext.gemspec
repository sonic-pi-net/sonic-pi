$:.push File.join(File.dirname(__FILE__), 'lib')

require 'concurrent/version'

Gem::Specification.new do |s|
  s.name        = 'concurrent-ruby-ext'
  s.version     = Concurrent::VERSION
  s.platform    = Gem::Platform::RUBY
  s.author      = "Jerry D'Antonio"
  s.email       = 'jerry.dantonio@gmail.com'
  s.homepage    = 'http://www.concurrent-ruby.com'
  s.summary     = 'C extensions to optimize concurrent-ruby under MRI.'
  s.license     = 'MIT'
  s.date        = Time.now.strftime('%Y-%m-%d')

  s.description = <<-EOF
    C extensions to optimize the concurrent-ruby gem when running under MRI.
    Please see http://concurrent-ruby.com for more information.
  EOF

  s.files            = Dir['ext/**/*.{h,c,cpp}']
  s.files           += [
    'lib/concurrent/atomic_reference/concurrent_update_error.rb',
    'lib/concurrent/atomic_reference/direct_update.rb',
    'lib/concurrent/atomic_reference/numeric_cas_wrapper.rb',
  ]
  s.extra_rdoc_files = Dir['README*', 'LICENSE*', 'CHANGELOG*']
  s.require_paths    = ['lib', 'ext']
  s.extensions       = 'ext/concurrent/extconf.rb'

  s.required_ruby_version = '>= 1.9.3'

  s.add_runtime_dependency 'concurrent-ruby', "~> #{Concurrent::VERSION}"
end
