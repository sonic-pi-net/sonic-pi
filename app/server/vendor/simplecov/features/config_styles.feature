@test_unit @config
Feature:

  There's several ways to configure SimpleCov. All of those
  config schemes below are equivalent and can be chosen by personal
  preference or project requirements.

  Background:
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      """

  Scenario: Inside start block
    Given a file named ".simplecov" with:
      """
      SimpleCov.start do
        add_filter 'test'
        command_name 'Config Test Runner'
      end
      """

    When I open the coverage report generated with `bundle exec rake test`
    Then I should see "4 files in total."
    And I should see "using Config Test Runner" within "#footer"

  Scenario: Inside start block, using instance var from outside
    Given a file named ".simplecov" with:
      """
      @filter = 'test'
      SimpleCov.start do
        add_filter @filter
        command_name 'Config Test Runner'
      end
      """

    When I open the coverage report generated with `bundle exec rake test`
    Then I should see "4 files in total."
    And I should see "using Config Test Runner" within "#footer"

  Scenario: Inside start block, using local var from outside
    Given a file named ".simplecov" with:
      """
      filter = 'test'
      SimpleCov.start do
        add_filter filter
        command_name 'Config Test Runner'
      end
      """

    When I open the coverage report generated with `bundle exec rake test`
    Then I should see "4 files in total."
    And I should see "using Config Test Runner" within "#footer"

  Scenario: Explicitly before start block
    Given a file named ".simplecov" with:
      """
      SimpleCov.add_filter 'test'
      SimpleCov.command_name 'Config Test Runner'
      SimpleCov.start
      """

    When I open the coverage report generated with `bundle exec rake test`
    Then I should see "4 files in total."
    And I should see "using Config Test Runner" within "#footer"

  Scenario: Explicitly after start block
    Given a file named ".simplecov" with:
      """
      SimpleCov.start
      SimpleCov.add_filter 'test'
      SimpleCov.command_name 'Config Test Runner'
      """

    When I open the coverage report generated with `bundle exec rake test`
    Then I should see "4 files in total."
    And I should see "using Config Test Runner" within "#footer"

  Scenario: Using configure block after start
    Given a file named ".simplecov" with:
      """
      SimpleCov.start
      SimpleCov.configure do
        add_filter 'test'
        command_name 'Config Test Runner'
      end
      """

    When I open the coverage report generated with `bundle exec rake test`
    Then I should see "4 files in total."
    And I should see "using Config Test Runner" within "#footer"

  Scenario: Using configure block before start
    Given a file named ".simplecov" with:
      """
      SimpleCov.configure do
        add_filter 'test'
        command_name 'Config Test Runner'
      end
      SimpleCov.start
      """

    When I open the coverage report generated with `bundle exec rake test`
    Then I should see "4 files in total."
    And I should see "using Config Test Runner" within "#footer"

  Scenario: Mixing configure and start block config
    Given a file named ".simplecov" with:
      """
      SimpleCov.configure do
        command_name 'Config Test Runner'
      end
      SimpleCov.start do
        add_filter 'test'
      end
      """

    When I open the coverage report generated with `bundle exec rake test`
    Then I should see "4 files in total."
    And I should see "using Config Test Runner" within "#footer"

