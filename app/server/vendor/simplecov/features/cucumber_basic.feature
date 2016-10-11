@cucumber
Feature:

  Simply adding the basic simplecov lines to a project should get
  the user a coverage report after running `cucumber features`

  Scenario:
    Given SimpleCov for Cucumber is configured with:
      """
      require 'simplecov'
      SimpleCov.start
      """

    When I open the coverage report generated with `bundle exec cucumber features`
    Then I should see the groups:
      | name      | coverage | files |
      | All Files | 91.23%   | 6     |

    And I should see the source files:
      | name                                    | coverage |
      | lib/faked_project.rb                    | 100.0 %  |
      | lib/faked_project/some_class.rb         | 80.0 %   |
      | lib/faked_project/framework_specific.rb | 75.0 %   |
      | lib/faked_project/meta_magic.rb         | 100.0 %  |
      | features/step_definitions/my_steps.rb   | 100.0 %  |
      | features/support/simplecov_config.rb    | 100.0 %  |

    And the report should be based upon:
      | Cucumber Features |
