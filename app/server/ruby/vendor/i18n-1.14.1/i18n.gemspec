# encoding: utf-8

$: << File.expand_path('../lib', __FILE__)
require 'i18n/version'

Gem::Specification.new do |s|
  s.name         = "i18n"
  s.version      = I18n::VERSION
  s.authors      = ["Sven Fuchs", "Joshua Harvey", "Matt Aimonetti", "Stephan Soller", "Saimon Moore", "Ryan Bigg"]
  s.email        = "rails-i18n@googlegroups.com"
  s.homepage     = "https://github.com/ruby-i18n/i18n"
  s.summary      = "New wave Internationalization support for Ruby"
  s.description  = "New wave Internationalization support for Ruby."
  s.license      = "MIT"

  s.metadata     = {
                     'bug_tracker_uri'   => 'https://github.com/ruby-i18n/i18n/issues',
                     'changelog_uri'     => 'https://github.com/ruby-i18n/i18n/releases',
                     'documentation_uri' => 'https://guides.rubyonrails.org/i18n.html',
                     'source_code_uri'   => 'https://github.com/ruby-i18n/i18n',
                   }

  s.files        = Dir.glob("lib/**/*") + %w(README.md MIT-LICENSE)
  s.platform     = Gem::Platform::RUBY
  s.require_path = 'lib'
  s.required_rubygems_version = '>= 1.3.5'
  s.required_ruby_version = '>= 2.3.0'

  s.add_dependency 'concurrent-ruby', '~> 1.0'

end
