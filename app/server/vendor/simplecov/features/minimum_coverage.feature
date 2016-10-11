@test_unit @config
Feature:

  Exit code should be non-zero if the overall coverage is below the
  minimum_coverage threshold.

  Scenario:
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      SimpleCov.start do
        add_filter 'test.rb'
        minimum_coverage 90
      end
      """

    When I run `bundle exec rake test`
    Then the exit status should not be 0
    And the output should contain "Coverage (88.10%) is below the expected minimum coverage (90.00%)."

  Scenario:
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      SimpleCov.start do
        add_filter 'test.rb'
        minimum_coverage 88.11
      end
      """

    When I run `bundle exec rake test`
    Then the exit status should not be 0
    And the output should contain "Coverage (88.10%) is below the expected minimum coverage (88.11%)."

  Scenario:
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      SimpleCov.start do
        add_filter 'test.rb'
        minimum_coverage 88.10
      end
      """

    When I run `bundle exec rake test`
    Then the exit status should be 0

  Scenario:
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      SimpleCov.start do
        add_filter 'test.rb'
      end
      """

    When I run `bundle exec rake test`
    Then the exit status should be 0

