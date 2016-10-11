@test_unit @unicode
Feature:

  Files with unicode in their source should be no problem at all for
  generating a proper coverage report.

  Background:
    Given SimpleCov for Test/Unit is configured with:
      """
      require 'simplecov'
      SimpleCov.start 'test_frameworks'
      """

  Scenario: Snowman inside method string
    Given a file named "lib/faked_project/unicode.rb" with:
      """
      # encoding: UTF-8
      class SourceCodeWithUnicode
        def self.yell!
          puts "☃"
        end
      end
      """

    When I open the coverage report generated with `bundle exec rake test`
    Then I should see the groups:
      | name      | coverage | files |
      | All Files | 86.67%   | 5     |

    And I should see the source files:
      | name                                    | coverage |
      | lib/faked_project.rb                    | 100.0 %  |
      | lib/faked_project/some_class.rb         | 80.0 %   |
      | lib/faked_project/framework_specific.rb | 75.0 %   |
      | lib/faked_project/meta_magic.rb         | 100.0 %  |
      | lib/faked_project/unicode.rb            | 66.67 %  |

    And the report should be based upon:
      | Unit Tests |

  Scenario: Author name in comment
    Given a file named "lib/faked_project/unicode.rb" with:
      """
      # encoding: UTF-8
      # author:  Javiér Hernández
      class SomeClassWrittenByAForeigner
        def self.yell!
          foo
        end
      end
      """

    When I open the coverage report generated with `bundle exec rake test`
    Then I should see the groups:
      | name      | coverage | files |
      | All Files | 86.67%   | 5     |

    And I should see the source files:
      | name                                    | coverage |
      | lib/faked_project.rb                    | 100.0 %  |
      | lib/faked_project/some_class.rb         | 80.0 %   |
      | lib/faked_project/framework_specific.rb | 75.0 %   |
      | lib/faked_project/meta_magic.rb         | 100.0 %  |
      | lib/faked_project/unicode.rb            | 66.67 %  |

    And the report should be based upon:
      | Unit Tests |
