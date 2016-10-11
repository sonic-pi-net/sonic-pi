@test_unit
Feature:

  Using the setting `tracked_files` should add files that were not
  required to the report.

  Scenario:
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      SimpleCov.start do
        track_files "lib/**/*.rb"
      end
      """

    When I open the coverage report generated with `bundle exec rake test`
    Then I should see the groups:
      | name      | coverage | files |
      | All Files | 76.81%   | 7     |

    And I should see the source files:
      | name                                    | coverage |
      | lib/faked_project.rb                    | 100.0 %  |
      | lib/faked_project/untested_class.rb     | 0.0 %    |
      | lib/faked_project/some_class.rb         | 80.0 %   |
      | lib/faked_project/framework_specific.rb | 75.0 %   |
      | lib/faked_project/meta_magic.rb         | 100.0 %  |
      | test/meta_magic_test.rb                 | 100.0 %  |
      | test/some_class_test.rb                 | 100.0 %  |
