require 'test_helper'

class I18nBackendPluralizationFallbackTest < I18n::TestCase
  class Backend < I18n::Backend::Simple
    include I18n::Backend::Pluralization
    include I18n::Backend::Fallbacks
  end

  def setup
    super
    I18n.default_locale = :'en'
    I18n.backend = Backend.new

    store_translations('en', cat: { zero: 'cat', one: 'cat', other: 'cats' })
    store_translations('en-US', cat: { zero: 'no cat', one: nil, other: 'lots of cats' })

    store_translations('ru', cat: { one: 'кот', few: 'кошек', many: 'кошка', other: 'кошек' })
    # probably not a real locale but just to demonstrate
    store_translations('ru-US', cat: { one: nil, few: nil, many: nil, other: nil })
    store_translations('ru', i18n: { plural: { rule: russian_rule }})
  end

  test "fallbacks: nils are ignored and fallback is applied" do
    assert_equal "no cat", I18n.t("cat", count: 0, locale: "en-US")
    assert_equal "cat", I18n.t("cat", count: 0, locale: "en")

    assert_equal "cat", I18n.t("cat", count: 1, locale: "en-US")
    assert_equal "cat", I18n.t("cat", count: 1, locale: "en")

    assert_equal "lots of cats", I18n.t("cat", count: 2, locale: "en-US")
    assert_equal "cats", I18n.t("cat", count: 2, locale: "en")
  end

  test "fallbacks: nils are ignored and fallback is applied, with custom rule" do
    # more specs: https://github.com/svenfuchs/rails-i18n/blob/master/spec/unit/pluralization/east_slavic.rb
    assert_equal "кошка", I18n.t("cat", count: 0, locale: "ru")
    assert_equal "кошка", I18n.t("cat", count: 0, locale: "ru-US")

    assert_equal "кот", I18n.t("cat", count: 1, locale: "ru")
    assert_equal "кот", I18n.t("cat", count: 1, locale: "ru-US")

    assert_equal "кошек", I18n.t("cat", count: 2, locale: "ru")
    assert_equal "кошек", I18n.t("cat", count: 2, locale: "ru-US")

    assert_equal "кошек", I18n.t("cat", count: 1.5, locale: "ru")
    assert_equal "кошек", I18n.t("cat", count: 1.5, locale: "ru-US")
  end

  private

  # copied from https://github.com/svenfuchs/rails-i18n/blob/master/lib/rails_i18n/common_pluralizations/east_slavic.rb
  def russian_rule
    lambda do |n|
      n ||= 0
      mod10 = n % 10
      mod100 = n % 100

      if mod10 == 1 && mod100 != 11
        :one
      elsif (2..4).include?(mod10) && !(12..14).include?(mod100)
        :few
      elsif mod10 == 0 || (5..9).include?(mod10) || (11..14).include?(mod100)
        :many
      else
        :other
      end
    end
  end
end
