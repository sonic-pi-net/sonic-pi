require 'rubygems'

Gem::Specification.new do |spec|
  spec.name       = 'sys-proctable'
  spec.version    = '1.1.3'
  spec.author     = 'Daniel J. Berger'
  spec.license    = 'Apache 2.0'
  spec.email      = 'djberg96@gmail.com'
  spec.homepage   = 'http://github.com/djberg96/sys-proctable'
  spec.platform   = Gem::Platform::CURRENT # Probably altered by Rake task
  spec.summary    = 'An interface for providing process table information'
  spec.test_files = ['test/test_sys_proctable_all.rb']
  spec.cert_chain = ['certs/djberg96_pub.pem']
   
  # Additional files for your platform are added by the 'rake gem' task.
  spec.files = [
    'benchmarks/bench_ps.rb',
    'examples/example_ps.rb',
    'lib/sys/proctable/version.rb',
    'lib/sys/top.rb',
    'CHANGES',
    'MANIFEST',
    'Rakefile',
    'README',
    'sys-proctable.gemspec'
  ]

  spec.extra_rdoc_files  = ['CHANGES', 'README', 'MANIFEST', 'doc/top.txt']

  spec.add_development_dependency('test-unit')
  spec.add_development_dependency('rake')

  spec.description = <<-EOF
    The sys-proctable library provides an interface for gathering information
    about processes on your system, i.e. the process table. Most major
    platforms are supported and, while different platforms may return
    different information, the external interface is identical across
    platforms.
  EOF
end
