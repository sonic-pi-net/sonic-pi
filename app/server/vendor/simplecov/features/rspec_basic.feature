@rspec
Feature:

  Simply adding the basic simplecov lines to a project should get
  the user a coverage report after running `rspec`

  Scenario:
    Given SimpleCov for RSpec is configured with:
      """
      require 'simplecov'
      SimpleCov.start
      """

    When I open the coverage report generated with `bundle exec rspec spec`
    Then I should see the groups:
      | name      | coverage | files |
      | All Files | 91.8%   | 7     |

    And I should see the source files:
      | name                                    | coverage |
      | lib/faked_project.rb                    | 100.0 %  |
      | lib/faked_project/some_class.rb         | 80.0 %   |
      | lib/faked_project/framework_specific.rb | 75.0 %   |
      | lib/faked_project/meta_magic.rb         | 100.0 %  |
      | spec/forking_spec.rb                    | 100.0 %  |
      | spec/meta_magic_spec.rb                 | 100.0 %  |
      | spec/some_class_spec.rb                 | 100.0 %  |

      # Note: faked_spec.rb is not appearing here since that's the first unit test file
      # loaded by Rake, and only there test_helper is required, which then loads simplecov
      # and triggers tracking of all other loaded files! Solution for this would be to
      # configure simplecov in this first test instead of test_helper.
