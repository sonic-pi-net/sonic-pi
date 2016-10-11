if RUBY_VERSION < "1.9"
  $stderr.puts "Sorry, Cucumber features are only meant to run on Ruby 1.9+ :("
  exit 0
end

require "bundler"
Bundler.setup
require "aruba/cucumber"
require "aruba/jruby" if RUBY_ENGINE == "jruby"
require "capybara/cucumber"
require "phantomjs/poltergeist"

# Fake rack app for capybara that just returns the latest coverage report from aruba temp project dir
Capybara.app = lambda { |env|
  request_path = env["REQUEST_PATH"] || "/"
  request_path = "/index.html" if request_path == "/"
  [
    200,
    {"Content-Type" => "text/html"},
    [File.read(File.join(File.dirname(__FILE__), "../../tmp/aruba/project/coverage", request_path))],
  ]
}

Capybara.default_driver = Capybara.javascript_driver = :poltergeist

Capybara.configure do |config|
  config.ignore_hidden_elements = false
end

Before do
  # JRuby takes it's time... See https://github.com/cucumber/aruba/issues/134
  @aruba_timeout_seconds = RUBY_ENGINE == "jruby" ? 60 : 20

  this_dir = File.dirname(__FILE__)

  # Clean up and create blank state for fake project
  in_current_directory do
    FileUtils.rm_rf "project"
    FileUtils.cp_r File.join(this_dir, "../../spec/faked_project/"), "project"
  end

  step 'I cd to "project"'
end

# Workaround for https://github.com/cucumber/aruba/pull/125
Aruba.configure do |config|
  config.before_cmd do
    set_env("JRUBY_OPTS", "--dev --debug")
  end
end
