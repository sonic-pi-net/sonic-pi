# -*- coding: utf-8 -*-

require 'gettext/mo'

class TestMo < Test::Unit::TestCase
  def test_not_exist_msgid
    mo = load_mo("_.mo")
    assert_equal(nil, mo["notexistent"])
  end

  def test_untranslated
    mo = load_mo("untranslated.mo")
    assert_false(mo.has_key?("untranslated"))
    assert_equal(nil, mo["untranslated"])
  end

  def test_non_ascii
    mo = load_mo("non_ascii.mo")
    assert_equal("Hello in Japanese", mo["こんにちは"])
  end

  def test_invalid_charset
    mo = load_mo("hello.mo", "ISO-8859-1")
    assert_equal("?????", mo["Hello"])
  end

  def test_backslash
    mo = load_mo("backslash.mo")
    assert_equal("'\\'は'\\\\'とエスケープするべきです。",
                 mo["You should escape '\\' as '\\\\'."])
  end

  def load_mo(file, output_charset=nil)
    output_charset ||= "UTF-8"
    GetText::MO.open("locale/ja/LC_MESSAGES/#{file}", output_charset)
  end
end
