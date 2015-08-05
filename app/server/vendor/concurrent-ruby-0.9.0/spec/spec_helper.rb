$VERBOSE = nil # suppress our deprecation warnings
require 'concurrent'
require 'concurrent-edge'

Concurrent.use_stdlib_logger Logger::FATAL

if ENV['COVERAGE'] || ENV['CI'] || ENV['TRAVIS']
  require 'simplecov'
  require 'coveralls'

  if ENV['TRAVIS']
    SimpleCov.formatter = SimpleCov::Formatter::MultiFormatter[
      SimpleCov::Formatter::HTMLFormatter,
      Coveralls::SimpleCov::Formatter
    ]
  else
    SimpleCov.formatter = SimpleCov::Formatter::HTMLFormatter
  end

  SimpleCov.start do
    project_name 'concurrent-ruby'
    add_filter '/build-tests/'
    add_filter '/coverage/'
    add_filter '/doc/'
    add_filter '/examples/'
    add_filter '/pkg/'
    add_filter '/spec/'
    add_filter '/tasks/'
    add_filter '/yard-template/'
    add_filter '/yardoc/'
  end
end

# import all the support files
Dir[File.join(File.dirname(__FILE__), 'support/**/*.rb')].each { |f| require File.expand_path(f) }

RSpec.configure do |config|
  #config.raise_errors_for_deprecations!
  config.order = 'random'
end
