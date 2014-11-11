source 'https://rubygems.org'

# Specify your gem's dependencies in did_you_mean.gemspec
gemspec

platforms :ruby do
  gem 'sqlite3'
end

platforms :jruby do
  gem 'activerecord-jdbcsqlite3-adapter'
end

platforms :rbx do
  gem 'rubysl', '~> 2.0'
  gem 'racc'
  gem 'rubinius-developer_tools'
end
