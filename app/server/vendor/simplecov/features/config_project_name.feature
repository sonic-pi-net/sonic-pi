@test_unit @config
Feature:

  SimpleCov guesses the project name from the project root dir's name.
  If this is not sufficient for you, you can specify a custom name using
  SimpleCov.project_name('xyz')


  Scenario: Guessed name
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      SimpleCov.start
      """

    When I open the coverage report generated with `bundle exec rake test`
    Then I should see "Code coverage for Project" within "title"

  Scenario: Custom name
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      SimpleCov.start { project_name "Superfancy 2.0" }
      """

    When I open the coverage report generated with `bundle exec rake test`
    Then I should see "Code coverage for Superfancy 2.0" within "title"
