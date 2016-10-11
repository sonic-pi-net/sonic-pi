@test_unit
Feature:

  Running unit tests without simplecov configuration

  Scenario: No config at all
    When I successfully run `bundle exec rake test`
    Then no coverage report should have been generated

  Scenario: Configured, but not started
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      SimpleCov.configure do
        add_filter 'somefilter'
      end
      """

    When I successfully run `bundle exec rake test`
    Then no coverage report should have been generated
