source 'https://rubygems.org'

gemspec name: 'concurrent-ruby'
gemspec name: 'concurrent-ruby-edge'

group :development do
  gem 'rake', '~> 10.4.2'
  gem 'rake-compiler', '~> 0.9.5'
  gem 'rake-compiler-dock', '~> 0.4.3'
  gem 'gem-compiler', '~> 0.3.0'
  gem 'benchmark-ips', '~> 2.2.0'

  # documentation
  gem 'countloc', '~> 0.4.0', :platforms => :mri, :require => false
  gem 'yard', '~> 0.8.7.6', :require => false
  gem 'inch', '~> 0.6.3', :platforms => :mri, :require => false
  gem 'redcarpet', '~> 3.3.2', platforms: :mri # understands github markdown
end

group :testing do
  gem 'rspec', '~> 3.3.0'
  gem 'timecop', '~> 0.7.4'

  # Coverage
  gem 'simplecov', '~> 0.10.0', :require => false
  gem 'coveralls', '~> 0.8.2', :require => false
end
