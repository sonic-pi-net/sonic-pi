@test_unit @nocov
Feature:

  When code is wrapped in :nocov: comment blocks, it does not count
  against the coverage numbers.

  Background:
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      SimpleCov.start 'test_frameworks'
      """

  Scenario: Plain run with a nocov'd method
    Given a file named "lib/faked_project/nocov.rb" with:
      """
      class SourceCodeWithNocov
        #:nocov:
        def some_weird_code
          never_reached
        rescue => err
          but no one cares about invalid ruby here
        end
        #:nocov:
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

  Scenario: Number of spaces should not mix up nocov results
    Given a file named "lib/faked_project/nocov.rb" with:
      """
      class SourceCodeWithNocov
           #    :nocov:
        def some_weird_code
          never_reached
        rescue => err
          but no one cares about invalid ruby here
        end
          #   :nocov:
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
