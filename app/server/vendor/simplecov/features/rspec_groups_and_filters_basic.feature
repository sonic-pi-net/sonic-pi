@rspec
Feature:

  Defining some groups and filters should give a corresponding
  coverage report that respects those settings after running rspec

  Scenario:
    Given SimpleCov for RSpec is configured with:
      """
      require 'simplecov'
      SimpleCov.start do
        add_group 'Libs', 'lib/faked_project/'
        add_filter '/spec/'
      end
      """

    When I open the coverage report generated with `bundle exec rspec spec`
    And I should see the groups:
      | name      | coverage | files |
      | All Files | 88.1%    | 4     |
      | Libs      | 86.11%   | 3     |
      | Ungrouped | 100.0%   | 1     |

    And I should see the source files:
      | name                                    | coverage |
      | lib/faked_project.rb                    | 100.0 %  |
      | lib/faked_project/some_class.rb         | 80.0 %  |
      | lib/faked_project/framework_specific.rb | 75.0 %   |
      | lib/faked_project/meta_magic.rb         | 100.0 %  |
