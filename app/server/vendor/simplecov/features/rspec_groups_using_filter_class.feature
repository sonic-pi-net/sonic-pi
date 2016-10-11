@rspec
Feature: Grouping on RSpec using a custom filter class

  Next to passing a block or a string to define a group, you can also pass
  a filter class. The filter class inherits from SimpleCov::Filter and
  must implement the matches? method, which is used to determine whether
  or not a file should be added to the group.

  Scenario:
    Given SimpleCov for RSpec is configured with:
      """
      require 'simplecov'
      class CoverageFilter < SimpleCov::Filter
        def matches?(source_file)
          source_file.covered_percent < filter_argument
        end
      end
      SimpleCov.start do
        add_group 'By filter class', CoverageFilter.new(90)
        add_group 'By string', 'project/meta_magic'
      end
      """

    When I open the coverage report generated with `bundle exec rspec spec`
    Then I should see the groups:
      | name             | coverage | files |
      | All Files        | 91.8%    | 7     |
      | By filter class  | 78.26%   | 2     |
      | By string        | 100.0%   | 1     |
      | Ungrouped        | 100.0%   | 4     |

    And I should see the source files:
      | name                                    | coverage |
      | lib/faked_project/framework_specific.rb | 75.0 %   |
      | lib/faked_project/some_class.rb         | 80.0 %   |
      | lib/faked_project.rb                    | 100.0 %  |
      | lib/faked_project/meta_magic.rb         | 100.0 %  |
      | spec/forking_spec.rb                    | 100.0 %  |
      | spec/meta_magic_spec.rb                 | 100.0 %  |
      | spec/some_class_spec.rb                 | 100.0 %  |

