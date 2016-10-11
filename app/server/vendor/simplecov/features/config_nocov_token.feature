@test_unit @nocov
Feature:

  Code wrapped in # :nocov: will be ignored by coverage reports.
  The name of the token can be configured with SimpleCov.nocov_token or SimpleCov.skip_token

  Scenario: Custom nocov token using nocov_token
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      SimpleCov.start 'test_frameworks' do
        nocov_token 'skippit'
      end
      """

    Given a file named "lib/faked_project/nocov.rb" with:
      """
      class SourceCodeWithNocov
        # :skippit:
        def some_weird_code
          never_reached
        rescue => err
          but no one cares about invalid ruby here
        end
        # :skippit:
      end
      """

    When I open the coverage report generated with `bundle exec rake test`

    Then I should see the source files:
      | name                                    | coverage |
      | lib/faked_project.rb                    | 100.0 %  |
      | lib/faked_project/some_class.rb         | 80.0 %   |
      | lib/faked_project/framework_specific.rb | 75.0 %   |
      | lib/faked_project/meta_magic.rb         | 100.0 %  |
      | lib/faked_project/nocov.rb              | 100.0 %  |

    And there should be 5 skipped lines in the source files

    And the report should be based upon:
      | Unit Tests |

  Scenario: Custom nocov token using skip_token
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      SimpleCov.start 'test_frameworks' do
        skip_token 'skippit'
      end
      """

    Given a file named "lib/faked_project/nocov.rb" with:
      """
      class SourceCodeWithNocov
        # :skippit:
        def some_weird_code
          never_reached
        rescue => err
          but no one cares about invalid ruby here
        end
        # :skippit:
      end
      """

    When I open the coverage report generated with `bundle exec rake test`

    Then I should see the source files:
      | name                                    | coverage |
      | lib/faked_project.rb                    | 100.0 %  |
      | lib/faked_project/some_class.rb         | 80.0 %   |
      | lib/faked_project/framework_specific.rb | 75.0 %   |
      | lib/faked_project/meta_magic.rb         | 100.0 %  |
      | lib/faked_project/nocov.rb              | 100.0 %  |

    And there should be 5 skipped lines in the source files

    And the report should be based upon:
      | Unit Tests |
