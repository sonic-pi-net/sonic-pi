@test_unit @config
Feature:

  Exit code should be non-zero if the overall coverage decreases by more than
  the maximum_coverage_drop threshold.

  Scenario:
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      SimpleCov.start do
        add_filter 'test.rb'
        maximum_coverage_drop 3.14
      end
      """

    When I run `bundle exec rake test`
    Then the exit status should be 0
    And a file named "coverage/.last_run.json" should exist

    Given a file named "lib/faked_project/missed.rb" with:
      """
      class UncoveredSourceCode
        def foo
          never_reached
        rescue => err
          but no one cares about invalid ruby here
        end
      end
      """

    When I run `bundle exec rake test`
    Then the exit status should not be 0
    And the output should contain "Coverage has dropped by 3.32% since the last time (maximum allowed: 3.14%)."
    And a file named "coverage/.last_run.json" should exist

