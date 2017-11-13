# encoding: UTF-8

require 'locale/info'
require 'test/unit'

class TestLocaleInfo < Test::Unit::TestCase
  def test_languages
    langs = Locale::Info.three_languages
    assert_equal 7600, langs.length
    assert_equal "English", langs["eng"].name
    assert langs["eng"].living?
    assert langs["eng"].individual?
    assert "ace", langs["ace"].to_s

    langs = Locale::Info.two_languages
    assert_equal 185, langs.length
    assert_equal "English", langs["en"].name
    assert langs["en"].living?
    assert langs["en"].individual?
  end

  def test_regions
    regions = Locale::Info.regions
    assert_equal 243, regions.length
    assert_equal "United States", regions["US"].name

    assert_equal "ÅLand Islands", regions['AX'].name
  end
end
