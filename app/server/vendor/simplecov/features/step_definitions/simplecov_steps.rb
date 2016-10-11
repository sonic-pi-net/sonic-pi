# Just a shortcut to make framework setup more readable
# The test project is using separate config files to avoid specifying all of
# test/spec_helper in the features every time.
Given /^SimpleCov for (.*) is configured with:$/ do |framework, config_body|
  framework_dir = begin
    case framework
    when /RSpec/i
      "spec"
    when /Test\/Unit/i
      "test"
    when /Cucumber/i
      "features/support"
    else
      raise ArgumentError, "Could not identify test framework #{framework}!"
    end
  end

  steps %(
    Given a file named "#{framework_dir}/simplecov_config.rb" with:
      """
      #{config_body}
      """
    )
end

When /^I open the coverage report generated with `([^`]+)`$/ do |command|
  steps %(
    When I successfully run `#{command}`
    Then a coverage report should have been generated
    When I open the coverage report
    )
end

Then /^a coverage report should have been generated(?: in "([^"]*)")?$/ do |coverage_dir|
  coverage_dir ||= "coverage"
  steps %(
    Then the output should contain "Coverage report generated"
    And a directory named "#{coverage_dir}" should exist
    And the following files should exist:
      | #{coverage_dir}/index.html      |
      | #{coverage_dir}/.resultset.json |
    )
end

Then /^no coverage report should have been generated(?: in "([^"]*)")?$/ do |coverage_dir|
  coverage_dir ||= "coverage"
  steps %(
    Then the output should not contain "Coverage report generated"
    And a directory named "#{coverage_dir}" should not exist
    And the following files should not exist:
      | #{coverage_dir}/index.html      |
      | #{coverage_dir}/.resultset.json |
    )
end

Then /^the report should be based upon:$/ do |table|
  frameworks = table.raw.flatten
  steps %(
    Then the output should contain "Coverage report generated for #{frameworks.join(', ')}"
    And I should see "using #{frameworks.join(', ')}" within "#footer"
    )
end

# This is neccessary to ensure timing-dependant tests like the merge timeout
# do not fail on powerful machines.
When /^I wait for (\d+) seconds$/ do |seconds|
  sleep seconds.to_i
end
