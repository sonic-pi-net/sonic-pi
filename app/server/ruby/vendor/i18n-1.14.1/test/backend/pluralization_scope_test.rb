require 'test_helper'

class I18nBackendPluralizationScopeTest < I18n::TestCase
  class Backend < I18n::Backend::Simple
    include I18n::Backend::Pluralization
    include I18n::Backend::Fallbacks
  end

  def setup
    super
    I18n.default_locale = :'en'
    I18n.backend = Backend.new

    translations = {
      i18n: {
        plural: {
          keys: [:one, :other],
          rule: lambda { |n| n == 1 ? :one : :other },
        }
      },
      activerecord: {
        models: {
          my_model: {
            one: 'one model',
            other: 'more models',
            some_other_key: {
              key: 'value'
            }
          }
        }
      }
    }

    store_translations('en', translations)
  end

  test "pluralization picks :other for 2" do
    args = {
      scope: [:activerecord, :models],
      count: 2,
      default: ["My model"]
    }
    assert_equal 'more models', I18n.translate(:my_model, **args)
  end

  test "pluralization picks :one for 1" do
    args = {
      scope: [:activerecord, :models],
      count: 1,
      default: ["My model"]
    }
    assert_equal 'one model', I18n.translate(:my_model, **args)
  end

end
