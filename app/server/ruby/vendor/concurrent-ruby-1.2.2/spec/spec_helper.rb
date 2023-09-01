if ENV['COVERAGE']
  require 'simplecov'
  require 'coveralls'

  SimpleCov.formatter = SimpleCov::Formatter::HTMLFormatter

  SimpleCov.start do
    project_name 'concurrent-ruby'
    add_filter '/examples/'
    add_filter '/spec/'
  end
end

if ENV['NO_PATH']
  # Patch rspec not to add lib to $LOAD_PATH, allows to test installed gems
  $LOAD_PATH.delete File.expand_path(File.join(__dir__, '..', 'lib'))
  class RSpec::Core::Configuration
    remove_method :requires=

    def requires=(paths)
      directories = [default_path].select { |p| File.directory? p }
      RSpec::Core::RubyProject.add_to_load_path(*directories)
      paths.each { |path| require path }
      @requires += paths
    end
  end
end

require_relative 'support/example_group_extensions'
require_relative 'support/threadsafe_test'

RSpec.configure do |config|
  #config.raise_errors_for_deprecations!
  config.filter_run_excluding stress: true
  config.order = 'random'
  config.disable_monkey_patching!
  config.example_status_persistence_file_path = 'spec/examples.txt'

  config.include Concurrent::TestHelpers
  config.extend Concurrent::TestHelpers

  config.before :all do
    # Only configure logging if it has been required, to make sure the necessary require's are in place
    if Concurrent.respond_to? :use_simple_logger
      Concurrent.use_simple_logger Logger::FATAL
    end
  end

  config.before :each do
    expect(!defined?(@created_threads) || @created_threads.nil? || @created_threads.empty?).to be_truthy
  end

  config.after :each do
    while defined?(@created_threads) && @created_threads && (thread = (@created_threads.pop(true) rescue nil))
      thread.kill
      thread_join = thread.join(1.0)
      expect(thread_join).not_to be_nil, thread.inspect
    end
  end
end

# Remove verbose and confusing "Pending: (Failures listed here ...)" section at the end.
# From https://github.com/rspec/rspec-core/issues/2377#issuecomment-275131981
module RSpecNoPendingOutput
  def dump_pending(_)
  end
end
RSpec::Core::Formatters::DocumentationFormatter.prepend RSpecNoPendingOutput
