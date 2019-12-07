source 'https://rubygems.org'

gem 'rake', '>= 0.9'
gem 'yard', '>= 0.8'

gem 'json',      '~> 1.4', :require => nil
gem 'json_pure', '~> 1.4', :require => nil

group :development do
  gem 'kramdown', '>= 0.14'
  gem 'pry'
  gem 'pry-debugger', :platforms => :mri
end

group :test do
  gem 'rspec', '>= 2.14'
  gem 'simplecov', :require => false
end

platforms :jruby do
  gem 'gson', '>= 0.6', :require => nil
  gem 'jrjackson', '~> 0.2.2', :require => nil
end

platforms :mingw, :mswin, :ruby do
  gem 'oj', '~> 2.0', :require => nil
  gem 'yajl-ruby', '~> 1.0', :require => nil
end

gemspec
