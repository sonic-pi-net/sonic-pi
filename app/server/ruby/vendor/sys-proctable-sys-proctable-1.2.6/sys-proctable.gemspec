require 'rubygems'

Gem::Specification.new do |spec|
  spec.name       = 'sys-proctable'
  spec.version    = '1.2.6'
  spec.author     = 'Daniel J. Berger'
  spec.license    = 'Apache-2.0'
  spec.email      = 'djberg96@gmail.com'
  spec.homepage   = 'http://github.com/djberg96/sys-proctable'
  spec.summary    = 'An interface for providing process table information'
  spec.test_files = FileList['spec/**/*.rb']
  spec.cert_chain = ['certs/djberg96_pub.pem']
   
  spec.files = FileList[
    "benchmarks/**/*.rb",
    "examples/**/*.rb",
    "lib/**/*.rb",
    'CHANGES.rdoc',
    'LICENSE',
    'MANIFEST.rdoc',
    'Rakefile',
    'README.md',
    'sys-proctable.gemspec'
  ]

  spec.extra_rdoc_files = ['CHANGES.rdoc', 'README.md', 'MANIFEST.rdoc']

  spec.add_dependency('ffi')
  spec.add_development_dependency('rspec')
  spec.add_development_dependency('rake')

  spec.metadata = {
    'homepage_uri'      => 'https://github.com/djberg96/sys-proctable',
    'bug_tracker_uri'   => 'https://github.com/djberg96/sys-proctable/issues',
    'changelog_uri'     => 'https://github.com/djberg96/sys-proctable/blob/master/CHANGES.rdoc',
    'documentation_uri' => 'https://github.com/djberg96/sys-proctable/wiki',
    'source_code_uri'   => 'https://github.com/djberg96/sys-proctable',
    'wiki_uri'          => 'https://github.com/djberg96/sys-proctable/wiki'
  }

  spec.description = <<-EOF
    The sys-proctable library provides an interface for gathering information
    about processes on your system, i.e. the process table. Most major
    platforms are supported and, while different platforms may return
    different information, the external interface is identical across
    platforms.
  EOF
end
