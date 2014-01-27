
require 'parslet'

require 'parslet/rig/rspec'
require 'parslet/atoms/visitor'
require 'parslet/export'

RSpec.configure do |config|
  config.mock_with :flexmock
  
  # Exclude other ruby versions by giving :ruby => 1.8 or :ruby => 1.9
  #
  config.filter_run_excluding :ruby => lambda { |version|
    RUBY_VERSION.to_s !~ /^#{Regexp.escape(version.to_s)}/
  }
end

def catch_failed_parse
  begin
    yield
  rescue Parslet::ParseFailed => exception
  end
  exception.cause
end