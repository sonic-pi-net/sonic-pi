require 'minitest/autorun'
require 'bundler/setup'
require 'i18n'
require 'mocha/setup'
require 'test_declarative'

class I18n::TestCase < Minitest::Test
  def assert_nothing_raised(*args)
    yield
  end

  def self.key_value?
    defined?(ActiveSupport)
  end

  def setup
    super
    I18n.load_path = nil
    I18n.enforce_available_locales = false
  end

  def teardown
    I18n.locale = nil
    I18n.default_locale = nil
    I18n.load_path = nil
    I18n.available_locales = nil
    I18n.backend = nil
    I18n.default_separator = nil
    I18n.enforce_available_locales = true
    I18n.fallbacks = nil if I18n.respond_to?(:fallbacks=)
    super
  end

  protected

  def translations
    I18n.backend.instance_variable_get(:@translations)
  end

  def store_translations(locale, data, options = I18n::EMPTY_HASH)
    I18n.backend.store_translations(locale, data, options)
  end

  def locales_dir
    File.dirname(__FILE__) + '/test_data/locales'
  end

  def stub_const(klass, constant, new_value)
    old_value = klass.const_get(constant)
    klass.send(:remove_const, constant)
    klass.const_set(constant, new_value)
    yield
  ensure
    klass.send(:remove_const, constant)
    klass.const_set(constant, old_value)
  end
end

class DummyRackApp
  def call(env)
    I18n.locale = :es
  end
end
