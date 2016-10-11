@test_unit @config
Feature:

  The formatter for test coverage can be customized
  with the SimpleCov.formatter setting. There are two
  built-in formatters:
    SimpleCov::Formatter::SimpleFormatter is a simple
    formatter returning a string of all files with
    theirs coverages.
    SimpleCov::Formatter::MultiFormatter is a formatter
    used to call multiple formatters at once.

  Scenario: With SimpleFormatter
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      SimpleCov.formatter = SimpleCov::Formatter::SimpleFormatter
      SimpleCov.at_exit do
        puts SimpleCov.result.format!
      end
      SimpleCov.start do
        add_group 'Libs', 'lib/faked_project/'
      end
      """

    When I successfully run `bundle exec rake test`
    Then the output should contain "lib/faked_project/meta_magic.rb (coverage: 100.0%)"

  Scenario: With MultiFormatter
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      SimpleCov.formatters = [
        SimpleCov::Formatter::SimpleFormatter,
        Class.new do
          def format(result)
            raise "Unable to format"
          end
        end
      ]

      SimpleCov.at_exit do
        puts SimpleCov.result.format!.join
      end
      SimpleCov.start do
        add_group 'Libs', 'lib/faked_project/'
      end
      """

    When I successfully run `bundle exec rake test`
    Then the output should contain "lib/faked_project/meta_magic.rb (coverage: 100.0%)"
    And the output should match /Formatter [^\s]* failed with RuntimeError: Unable to format/

  Scenario: With multiple formatters
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      SimpleCov.formatters = [
        SimpleCov::Formatter::SimpleFormatter,
        Class.new do
          def format(result)
            raise "Unable to format"
          end
        end
      ]

      SimpleCov.at_exit do
        puts SimpleCov.result.format!.join
      end
      SimpleCov.start do
        add_group 'Libs', 'lib/faked_project/'
      end
      """

    When I successfully run `bundle exec rake test`
    Then the output should contain "lib/faked_project/meta_magic.rb (coverage: 100.0%)"
    And the output should match /Formatter [^\s]* failed with RuntimeError: Unable to format/
