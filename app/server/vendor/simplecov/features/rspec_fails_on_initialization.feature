@rspec
Feature:

  Running specs with a failing rspec setup

  Scenario: Fail if rspec fails before starting its tests
    Given a file named "spec/spec_helper.rb" with:
      """
      require 'simplecov'
      SimpleCov.start
      raise "some exception in the class loading before the tests start"
      """
    When I run `bundle exec rspec spec`
    Then the exit status should not be 0
