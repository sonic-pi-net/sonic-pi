@test_unit @rspec @config
Feature:

  If you have multiple test suites, it can be a bit cumbersome
  to keep the configuration across them in sync. SimpleCov
  is able to find a config file called '.simplecov' that resides
  in your project's root and will automatically use it when
  loaded.

  This gives you the ability to configure SimpleCov just once
  and then use the same configuration on all test suites simply
  by doing a 'require "simplecov"'

  Scenario:
    Given a file named ".simplecov" with:
      """
      SimpleCov.start do
        add_filter 'test.rb'
        add_filter 'spec.rb'
      end
      """
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      """
    Given SimpleCov for RSpec is configured with:
      """
      require 'simplecov'
      """

    When I successfully run `bundle exec rake test`
    And I open the coverage report generated with `bundle exec rspec spec`
    Then the report should be based upon:
      | RSpec      |
      | Unit Tests |

    And I should see the groups:
      | name      | coverage | files |
      | All Files | 90.48%    | 4     |

    And I should see the source files:
      | name                                    | coverage |
      | lib/faked_project.rb                    | 100.0 %  |
      | lib/faked_project/some_class.rb         |  80.0 %  |
      | lib/faked_project/framework_specific.rb |  87.5 %  |
      | lib/faked_project/meta_magic.rb         | 100.0 %  |
