require "bundler/setup"

# We're injecting simplecov_config via aruba in cucumber here
# depending on what the test case is...
begin
  require File.join(File.dirname(__FILE__), "simplecov_config")
rescue LoadError
  $stderr.puts "No SimpleCov config file found!"
end

require "faked_project"
