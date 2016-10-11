@test_unit @rspec @merging @config
Feature:

  The maximum time between resultset merges can be customized
  using SimpleCov.merge_timeout SECONDS. This can be helpful for
  long-running test-suites that fail to merge because of the time
  between individual suite finishes is more then the default timeout
  of 10 minutes.

  Here, for the sake of testing the opposite case is shown, choosing
  a merge timeout so short that the first test suite's results actually
  are out of date when the second suite finishes and thus does not end up
  in the report.

  Scenario:
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      SimpleCov.start do
        merge_timeout 5
      end
      """
    Given SimpleCov for RSpec is configured with:
      """
      require 'simplecov'
      SimpleCov.start do
        merge_timeout 5
      end
      """

    When I open the coverage report generated with `bundle exec rake test`
    Then the report should be based upon:
      | Unit Tests |

    When I wait for 5 seconds
    And I open the coverage report generated with `bundle exec rspec spec`
    Then the report should be based upon:
      | RSpec |

