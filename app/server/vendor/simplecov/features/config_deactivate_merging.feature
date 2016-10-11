@test_unit @rspec @merging @config
Feature:

  If merging of test suite results is not desired, it can be deactivated,
  thus leading to the coverage report being overwritten with the latest results
  of a single test suite on each run of any suite.

  It's probably preferrable to generate the individual suite results into separate
  output directories instead (see SimpleCov.coverage_dir), but it is possible :)

  Scenario:
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      SimpleCov.start do
        use_merging false
      end
      """
    Given SimpleCov for RSpec is configured with:
      """
      require 'simplecov'
      SimpleCov.start do
        use_merging false
      end
      """

    When I successfully run `bundle exec rake test`
    Then a file named "coverage/index.html" should exist
      But a file named "coverage/.resultset.json" should not exist

    Given I open the coverage report
    Then the report should be based upon:
      | Unit Tests |

    When I successfully run `bundle exec rspec spec`
    Then a file named "coverage/index.html" should exist
      But a file named "coverage/.resultset.json" should not exist

    Given I open the coverage report
    Then the report should be based upon:
      | RSpec |

