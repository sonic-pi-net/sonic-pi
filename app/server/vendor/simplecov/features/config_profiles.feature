@test_unit @config @profiles
Feature:

  In order to re-use SimpleCov settings across projects,
  profiles can be defined that hold configuration settings
  that can be loaded at once.

  Background:
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      """

  Scenario: Defining and using a custom profile
    Given a file named ".simplecov" with:
      """
      SimpleCov.profiles.define 'custom_command' do
        command_name "Profile Command"
      end

      SimpleCov.start do
        load_profile 'test_frameworks'
        load_profile 'custom_command'
      end
      """

    When I open the coverage report generated with `bundle exec rake test`
    Then I should see "4 files in total."
    And I should see "using Profile Command" within "#footer"

  Scenario: Using existing profile in custom profile and supplying profile to start command
    Given a file named ".simplecov" with:
      """
      SimpleCov.profiles.define 'my_profile' do
        load_profile 'test_frameworks'
        command_name "My Profile"
      end

      SimpleCov.start 'my_profile'
      """

    When I open the coverage report generated with `bundle exec rake test`
    Then I should see "4 files in total."
    And I should see "using My Profile" within "#footer"
