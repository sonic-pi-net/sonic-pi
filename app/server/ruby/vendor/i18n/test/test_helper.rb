$KCODE = 'u' if RUBY_VERSION <= '1.9'

require 'rubygems'

# Use minitest if we can, otherwise fallback to test-unit.
begin
  require 'minitest/autorun'
  TEST_CASE = defined?(Minitest::Test) ? Minitest::Test : MiniTest::Unit::TestCase

  # TODO: Remove these aliases and update tests accordingly.
  class TEST_CASE
    alias :assert_raise :assert_raises
    alias :assert_not_equal :refute_equal

    def assert_nothing_raised(*args)
      yield
    end
  end
rescue LoadError
  require 'test/unit'
  TEST_CASE = Test::Unit::TestCase
end

# Do not load the i18n gem from libraries like active_support.
#
# This is required for testing against Rails 2.3 because active_support/vendor.rb#24 tries
# to load I18n using the gem method. Instead, we want to test the local library of course.
alias :gem_for_ruby_19 :gem # for 1.9. gives a super ugly seg fault otherwise
def gem(gem_name, *version_requirements)
  gem_name =='i18n' ? puts("skipping loading the i18n gem ...") : super
end

require 'bundler/setup'
require 'i18n'
require 'mocha/setup'
require 'test_declarative'

class I18n::TestCase < TEST_CASE
  def self.setup_rufus_tokyo
    require 'rufus/tokyo'
  rescue LoadError => e
    puts "can't use KeyValue backend because: #{e.message}"
  end

  def teardown
    super
    I18n.locale = nil
    I18n.default_locale = :en
    I18n.load_path = []
    I18n.available_locales = nil
    I18n.backend = nil
    I18n.enforce_available_locales = nil
  end

  # Ignore Test::Unit::TestCase failing if the test case does not contain any
  # test, otherwise it will blow up because of this base class.
  #
  # TODO: remove when test-unit is not used anymore.
  def default_test
    nil
  end

  protected

  def translations
    I18n.backend.instance_variable_get(:@translations)
  end

  def store_translations(locale, data)
    I18n.backend.store_translations(locale, data)
  end

  def locales_dir
    File.dirname(__FILE__) + '/test_data/locales'
  end
end
