@test_unit
Feature:

  Simply adding the basic simplecov lines to a project should get
  the user a coverage report after running `rake test`

  Scenario:
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      SimpleCov.start
      """

    When I open the coverage report generated with `bundle exec rake test`
    Then I should see the groups:
      | name      | coverage | files |
      | All Files | 91.38%   | 6     |

    And I should see the source files:
      | name                                    | coverage |
      | lib/faked_project.rb                    | 100.0 %  |
      | lib/faked_project/some_class.rb         | 80.0 %   |
      | lib/faked_project/framework_specific.rb | 75.0 %   |
      | lib/faked_project/meta_magic.rb         | 100.0 %  |
      | test/meta_magic_test.rb                 | 100.0 %  |
      | test/some_class_test.rb                 | 100.0 %  |

      # Note: faked_test.rb is not appearing here since that's the first unit test file
      # loaded by Rake, and only there test_helper is required, which then loads simplecov
      # and triggers tracking of all other loaded files! Solution for this would be to
      # configure simplecov in this first test instead of test_helper.

    And the report should be based upon:
      | Unit Tests |
