require 'locale/tag'
require 'test/unit'

class TagTest < Test::Unit::TestCase

  def test_simple_lang
    #ja-JP, ja-392
    lang = Locale::Tag.parse("ja")
    assert_equal Locale::Tag::Simple, lang.class
    assert_equal "ja", lang.language
    assert_equal nil, lang.region
    assert_equal [Locale::Tag::Simple.parse("ja"),
                  Locale::Tag::Simple.parse("ja")], lang.candidates

    assert_equal Locale::Tag::Simple.parse("ja"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("ja"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("ja"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("ja"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("ja"), lang.to_posix

    lang = Locale::Tag.parse("kok")
    assert_equal Locale::Tag::Simple, lang.class
    assert_equal "kok", lang.language
    assert_equal nil, lang.region
    assert_equal [Locale::Tag::Simple.parse("kok"),
                  Locale::Tag::Simple.parse("kok")], lang.candidates
    
    assert_equal Locale::Tag::Simple.parse("kok"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("kok"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("kok"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("kok"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("kok"), lang.to_posix
  end

  def test_simple_lang_region
    lang = Locale::Tag.parse("ja-JP")
    assert_equal Locale::Tag::Simple, lang.class
    assert_equal "ja", lang.language
    assert_equal "JP", lang.region
    assert_equal [Locale::Tag::Simple.parse("ja_JP"),
                  Locale::Tag::Simple.parse("ja")], lang.candidates

    assert_equal Locale::Tag::Simple.parse("ja_JP"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("ja_JP"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("ja-JP"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("ja-JP"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("ja_JP"), lang.to_posix

    lang = Locale::Tag.parse("ja_JP")
    assert_equal Locale::Tag::Simple, lang.class
    assert_equal "ja", lang.language
    assert_equal "JP", lang.region
    assert_equal [Locale::Tag::Simple.parse("ja_JP"),
                  Locale::Tag::Simple.parse("ja")], lang.candidates

    assert_equal Locale::Tag::Simple.parse("ja_JP"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("ja_JP"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("ja-JP"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("ja-JP"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("ja_JP"), lang.to_posix
  end

  def test_simple_unm49
    # UN_M.49(Country code)
    lang = Locale::Tag.parse("ja-392")
    assert_equal Locale::Tag::Simple, lang.class
    assert_equal "ja", lang.language
    assert_equal "392", lang.region
    assert_equal [Locale::Tag::Simple.parse("ja_392"),
                  Locale::Tag::Simple.parse("ja")], lang.candidates

    assert_equal Locale::Tag::Simple.parse("ja-392"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("ja-392"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("ja-392"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("ja-392"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("ja_392"), lang.to_posix
    
    lang = Locale::Tag.parse("jpn-392")
    assert_equal Locale::Tag::Simple, lang.class
    assert_equal "jpn", lang.language
    assert_equal "392", lang.region
    assert_equal [Locale::Tag::Simple.parse("jpn-392"),
                  Locale::Tag::Simple.parse("jpn")], lang.candidates

    assert_equal Locale::Tag::Simple.parse("jpn-392"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("jpn-392"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("jpn-392"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("jpn-392"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("jpn_392"), lang.to_posix

    lang = Locale::Tag.parse("es-419")
    assert_equal Locale::Tag::Simple, lang.class
    assert_equal "es", lang.language
    assert_equal "419", lang.region
    assert_equal [Locale::Tag::Simple.parse("es-419"),
                  Locale::Tag::Simple.parse("es")], lang.candidates

    assert_equal Locale::Tag::Simple.parse("es-419"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("es-419"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("es-419"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("es_419"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("es_419"), lang.to_posix
  end

  def test_simple_writer
    lang = Locale::Tag.parse("ja-JP")
    assert_equal Locale::Tag::Simple, lang.class
    assert_equal "ja_JP", lang.to_s
    assert_equal [Locale::Tag::Simple.parse("ja-JP"),
                  Locale::Tag::Simple.parse("ja")], lang.candidates
    lang.language = "en"
    assert_equal "en", lang.language
    assert_equal "en_JP", lang.to_s
    assert_equal [Locale::Tag::Simple.parse("en-JP"),
                  Locale::Tag::Simple.parse("en")], lang.candidates

    lang.region = "US"
    assert_equal "en", lang.language
    assert_equal "US", lang.region
    assert_equal "en_US", lang.to_s
    assert_equal [Locale::Tag::Simple.parse("en-US"),
                  Locale::Tag::Simple.parse("en")], lang.candidates

  end

  #lang, region, script, variants
  def test_common_lang_script
    lang = Locale::Tag.parse("az_Arab")
    assert_equal Locale::Tag::Common, lang.class
    assert_equal "az", lang.language
    assert_equal nil, lang.region
    assert_equal "Arab", lang.script
    assert_equal [Locale::Tag::Common.parse("az_Arab"),
                  Locale::Tag::Common.parse("az_Arab"),
                  Locale::Tag::Common.parse("az"),
                  Locale::Tag::Common.parse("az"),
                  Locale::Tag::Common.parse("az_Arab"),
                  Locale::Tag::Common.parse("az_Arab"),
                  Locale::Tag::Common.parse("az"),
                  Locale::Tag::Common.parse("az"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("az"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("az_Arab"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("az-Arab"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("az_Arab"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("az"), lang.to_posix


    lang = Locale::Tag.parse("uz-Cyrl")
    assert_equal Locale::Tag::Common, lang.class
    assert_equal "uz", lang.language
    assert_equal nil, lang.region
    assert_equal "Cyrl", lang.script
    assert_equal [Locale::Tag::Common.parse("uz-Cyrl"),
                  Locale::Tag::Common.parse("uz-Cyrl"),
                  Locale::Tag::Common.parse("uz"),
                  Locale::Tag::Common.parse("uz"),
                  Locale::Tag::Common.parse("uz-Cyrl"),
                  Locale::Tag::Common.parse("uz-Cyrl"),
                  Locale::Tag::Common.parse("uz"),
                  Locale::Tag::Common.parse("uz"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("uz"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("uz_Cyrl"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("uz-Cyrl"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("uz_Cyrl"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("uz"), lang.to_posix
  end

  def test_common_lang_script_region
    lang = Locale::Tag.parse("ja-Kana-JP")
    assert_equal Locale::Tag::Common, lang.class
    assert_equal "ja", lang.language
    assert_equal "JP", lang.region
    assert_equal "Kana", lang.script
    assert_equal [Locale::Tag::Common.parse("ja-Kana-JP"),
                  Locale::Tag::Common.parse("ja-Kana-JP"),
                  Locale::Tag::Common.parse("ja-JP"),
                  Locale::Tag::Common.parse("ja-JP"),
                  Locale::Tag::Common.parse("ja-Kana"),
                  Locale::Tag::Common.parse("ja-Kana"),
                  Locale::Tag::Common.parse("ja"),
                  Locale::Tag::Common.parse("ja")], lang.candidates

    assert_equal Locale::Tag::Simple.parse("ja_JP"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("ja_Kana_JP"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("ja-Kana-JP"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("ja_Kana_JP"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("ja_JP"), lang.to_posix

    # 3 char language
    lang = Locale::Tag.parse("jpn-Hira-JP")
    assert_equal Locale::Tag::Common, lang.class
    assert_equal "jpn", lang.language
    assert_equal "JP", lang.region
    assert_equal "Hira", lang.script
    assert_equal [Locale::Tag::Common.parse("jpn-Hira-JP"),
                  Locale::Tag::Common.parse("jpn-Hira-JP"),
                  Locale::Tag::Common.parse("jpn-JP"),
                  Locale::Tag::Common.parse("jpn-JP"),
                  Locale::Tag::Common.parse("jpn-Hira"),
                  Locale::Tag::Common.parse("jpn-Hira"),
                  Locale::Tag::Common.parse("jpn"),
                  Locale::Tag::Common.parse("jpn")], lang.candidates

    assert_equal Locale::Tag::Simple.parse("jpn_JP"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("jpn_Hira_JP"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("jpn-Hira-JP"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("jpn_Hira_JP"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("jpn_JP"), lang.to_posix
  end

  def test_common_lang_script_unm49
    lang = Locale::Tag.parse("jpn-Hira-392")
    assert_equal Locale::Tag::Common, lang.class
    assert_equal "jpn", lang.language
    assert_equal "392", lang.region
    assert_equal "Hira", lang.script
    assert_equal [Locale::Tag::Common.parse("jpn_Hira_392"),
                  Locale::Tag::Common.parse("jpn_Hira_392"),
                  Locale::Tag::Common.parse("jpn_392"),
                  Locale::Tag::Common.parse("jpn_392"),
                  Locale::Tag::Common.parse("jpn_Hira"),
                  Locale::Tag::Common.parse("jpn_Hira"),
                  Locale::Tag::Common.parse("jpn"),
                  Locale::Tag::Common.parse("jpn")], lang.candidates


    assert_equal Locale::Tag::Simple.parse("jpn_392"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("jpn_Hira_392"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("jpn-Hira-392"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("jpn_Hira_392"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("jpn_392"), lang.to_posix
  end

  def test_common_lang_script_region_variants
    lang = Locale::Tag.parse("en_Latn_US_NYNORSK")
    assert_equal Locale::Tag::Common, lang.class
    assert_equal "en", lang.language
    assert_equal "US", lang.region
    assert_equal ["NYNORSK"], lang.variants
    assert_equal [Locale::Tag::Common.parse("en_Latn_US_NYNORSK"),
                  Locale::Tag::Common.parse("en_Latn_US"),
                  Locale::Tag::Common.parse("en_US_NYNORSK"),
                  Locale::Tag::Common.parse("en_US"),
                  Locale::Tag::Common.parse("en_Latn_NYNORSK"),
                  Locale::Tag::Common.parse("en_Latn"),
                  Locale::Tag::Common.parse("en_NYNORSK"),
                  Locale::Tag::Common.parse("en")], lang.candidates


    assert_equal Locale::Tag::Simple.parse("en-US"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("en_Latn_US_NYNORSK"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("en-Latn-US-NYNORSK"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("en_Latn_US_NYNORSK"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("en_US@NYNORSK"), lang.to_posix
  end


  def test_common_lang_variants
    lang = Locale::Tag.parse("ja-osaka")
    assert_equal Locale::Tag::Common, lang.class
    assert_equal "ja", lang.language
    assert_equal nil, lang.region
    assert_equal nil, lang.script
    assert_equal ["osaka"], lang.variants

    assert_equal [Locale::Tag::Common.parse("ja-osaka"),
                  Locale::Tag::Common.parse("ja"),
                  Locale::Tag::Common.parse("ja-osaka"),
                  Locale::Tag::Common.parse("ja"),
                  Locale::Tag::Common.parse("ja-osaka"),
                  Locale::Tag::Common.parse("ja"),
                  Locale::Tag::Common.parse("ja-osaka"),
                  Locale::Tag::Common.parse("ja")
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("ja"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("ja-osaka"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("ja-osaka"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("ja_OSAKA"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("ja@osaka"), lang.to_posix
  end

  def test_common_lang_region_variants
    lang = Locale::Tag.parse("ja-JP-osaka")
    assert_equal Locale::Tag::Common, lang.class
    assert_equal "ja", lang.language
    assert_equal "JP", lang.region
    assert_equal nil, lang.script
    assert_equal ["osaka"], lang.variants

    assert_equal [Locale::Tag::Common.parse("ja_JP_osaka"),
                  Locale::Tag::Common.parse("ja_JP"),
                  Locale::Tag::Common.parse("ja_JP_osaka"),
                  Locale::Tag::Common.parse("ja_JP"),
                  Locale::Tag::Common.parse("ja_osaka"),
                  Locale::Tag::Common.parse("ja"),
                  Locale::Tag::Common.parse("ja_osaka"),
                  Locale::Tag::Common.parse("ja")
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("ja_JP"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("ja_JP_osaka"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("ja-JP-osaka"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("ja_JP_OSAKA"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("ja_JP@osaka"), lang.to_posix
  end

  def test_common_lang_script_region_5length_variants
    lang = Locale::Tag.parse("ja-Kana-JP-osaka")
    assert_equal Locale::Tag::Common, lang.class
    assert_equal "ja", lang.language
    assert_equal "JP", lang.region
    assert_equal "Kana", lang.script
    assert_equal ["osaka"], lang.variants

    assert_equal [Locale::Tag::Common.parse("ja-Kana-JP-osaka"),
                  Locale::Tag::Common.parse("ja-Kana-JP"),
                  Locale::Tag::Common.parse("ja-JP-osaka"),
                  Locale::Tag::Common.parse("ja-JP"),
                  Locale::Tag::Common.parse("ja-Kana-osaka"),
                  Locale::Tag::Common.parse("ja-Kana"),
                  Locale::Tag::Common.parse("ja-osaka"),
                  Locale::Tag::Common.parse("ja")
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("ja_JP"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("ja_Kana-JP_osaka"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("ja-Kana-JP-osaka"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("ja_Kana_JP_OSAKA"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("ja_JP@osaka"), lang.to_posix
  end

  def test_common_lang_script_region_mid_length_variants
    #middle length
    lang = Locale::Tag.parse("ja-Kana-JP-aomori")
    assert_equal Locale::Tag::Common, lang.class
    assert_equal "ja", lang.language
    assert_equal "JP", lang.region
    assert_equal "Kana", lang.script
    assert_equal ["aomori"], lang.variants

    assert_equal [Locale::Tag::Common.parse("ja-Kana-JP-aomori"),
                  Locale::Tag::Common.parse("ja-Kana-JP"),
                  Locale::Tag::Common.parse("ja-JP-aomori"),
                  Locale::Tag::Common.parse("ja-JP"),
                  Locale::Tag::Common.parse("ja-Kana-aomori"),
                  Locale::Tag::Common.parse("ja-Kana"),
                  Locale::Tag::Common.parse("ja-aomori"),
                  Locale::Tag::Common.parse("ja")
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("ja_JP"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("ja_Kana-JP_aomori"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("ja-Kana-JP-aomori"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("ja_Kana_JP_AOMORI"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("ja_JP@aomori"), lang.to_posix

  end

  def test_common_lang_script_region_8length_variants
    #8 length
    lang = Locale::Tag.parse("ja-Kana-JP-hokuriku")
    assert_equal Locale::Tag::Common, lang.class
    assert_equal "ja", lang.language
    assert_equal "JP", lang.region
    assert_equal "Kana", lang.script
    assert_equal ["hokuriku"], lang.variants

    assert_equal [Locale::Tag::Common.parse("ja-Kana-JP-hokuriku"),
                  Locale::Tag::Common.parse("ja-Kana-JP"),
                  Locale::Tag::Common.parse("ja-JP-hokuriku"),
                  Locale::Tag::Common.parse("ja-JP"),
                  Locale::Tag::Common.parse("ja-Kana-hokuriku"),
                  Locale::Tag::Common.parse("ja-Kana"),
                  Locale::Tag::Common.parse("ja-hokuriku"),
                  Locale::Tag::Common.parse("ja")
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("ja_JP"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("ja_Kana-JP_hokuriku"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("ja-Kana-JP-hokuriku"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("ja_Kana_JP_HOKURIKU"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("ja_JP@hokuriku"), lang.to_posix
  end

  def test_common_lang_script_region_special_variants
    #1 digit and 3 alpha
    lang = Locale::Tag.parse("ja-Kana-JP-0abc")
    assert_equal Locale::Tag::Common, lang.class
    assert_equal "ja", lang.language
    assert_equal "JP", lang.region
    assert_equal "Kana", lang.script
    assert_equal ["0abc"], lang.variants

    assert_equal [Locale::Tag::Common.parse("ja-Kana-JP-0abc"),
                  Locale::Tag::Common.parse("ja-Kana-JP"),
                  Locale::Tag::Common.parse("ja-JP-0abc"),
                  Locale::Tag::Common.parse("ja-JP"),
                  Locale::Tag::Common.parse("ja-Kana-0abc"),
                  Locale::Tag::Common.parse("ja-Kana"),
                  Locale::Tag::Common.parse("ja-0abc"),
                  Locale::Tag::Common.parse("ja")
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("ja_JP"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("ja_Kana-JP_0abc"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("ja-Kana-JP-0abc"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("ja_Kana_JP_0ABC"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("ja_JP@0abc"), lang.to_posix
  end

  def test_common_lang_region_4digits_variants
    #4 digits
    lang = Locale::Tag.parse("de-CH-1996")
    assert_equal Locale::Tag::Common, lang.class
    assert_equal "de", lang.language
    assert_equal "CH", lang.region
    assert_equal nil, lang.script
    assert_equal ["1996"], lang.variants
    assert_equal [Locale::Tag::Common.parse("de_CH_1996"),
                  Locale::Tag::Common.parse("de_CH"),
                  Locale::Tag::Common.parse("de_CH_1996"),
                  Locale::Tag::Common.parse("de_CH"),
                  Locale::Tag::Common.parse("de_1996"),
                  Locale::Tag::Common.parse("de"),     
                  Locale::Tag::Common.parse("de_1996"),
                  Locale::Tag::Common.parse("de"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("de-CH"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("de-CH-1996"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("de-CH-1996"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("de_CH_1996"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("de_CH@1996"), lang.to_posix
  end

  def test_common_lang_region_plural_variants
    #Plural
    lang = Locale::Tag.parse("ja-Kana-JP-hokuriku-aomori")
    assert_equal Locale::Tag::Common, lang.class
    assert_equal "ja", lang.language
    assert_equal "JP", lang.region
    assert_equal "Kana", lang.script
    assert_equal ["hokuriku", "aomori"], lang.variants

    assert_equal [Locale::Tag::Common.parse("ja-Kana-JP-hokuriku-aomori"),
                  Locale::Tag::Common.parse("ja-Kana-JP"),
                  Locale::Tag::Common.parse("ja-JP-hokuriku-aomori"),
                  Locale::Tag::Common.parse("ja-JP"),
                  Locale::Tag::Common.parse("ja-Kana-hokuriku-aomori"),
                  Locale::Tag::Common.parse("ja-Kana"),
                  Locale::Tag::Common.parse("ja-hokuriku-aomori"),
                  Locale::Tag::Common.parse("ja"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("ja-JP"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("ja-Kana-JP-hokuriku-aomori"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("ja-Kana-JP-hokuriku-aomori"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("ja-Kana-JP_HOKURIKU_AOMORI"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("ja_JP@hokuriku-aomori"), lang.to_posix
  end

  def test_common_cases
    #cases
    lang = Locale::Tag.parse("mn-Cyrl-MN")
    assert_equal Locale::Tag::Common, lang.class
    assert_equal "mn", lang.language
    assert_equal "MN", lang.region
    assert_equal "Cyrl", lang.script

    assert_equal [Locale::Tag::Common.parse("mn-Cyrl-MN"),
                  Locale::Tag::Common.parse("mn-Cyrl-MN"),
                  Locale::Tag::Common.parse("mn-MN"),
                  Locale::Tag::Common.parse("mn-MN"),
                  Locale::Tag::Common.parse("mn-Cyrl"),
                  Locale::Tag::Common.parse("mn-Cyrl"),
                  Locale::Tag::Common.parse("mn"),
                  Locale::Tag::Common.parse("mn"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("mn_MN"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("mn_Cyrl_MN"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("mn-Cyrl-MN"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("mn-Cyrl-MN"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("mn_MN"), lang.to_posix

    lang = Locale::Tag.parse("mN-cYrL-Mn")
    assert_equal Locale::Tag::Common, lang.class
    assert_equal "mn", lang.language
    assert_equal "MN", lang.region
    assert_equal "Cyrl", lang.script

    assert_equal Locale::Tag::Simple.parse("mn_MN"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("mn_Cyrl_MN"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("mn-Cyrl-MN"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("mn-Cyrl-MN"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("mn_MN"), lang.to_posix

    lang = Locale::Tag.parse("MN-cYRL-mn")
    assert_equal Locale::Tag::Common, lang.class
    assert_equal "mn", lang.language
    assert_equal "MN", lang.region
    assert_equal "Cyrl", lang.script

    assert_equal Locale::Tag::Simple.parse("mn_MN"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("mn_Cyrl_MN"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("mn-Cyrl-MN"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("mn-Cyrl-MN"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("mn_MN"), lang.to_posix

    lang = Locale::Tag.parse("zh-Latn-CN-variant1")
    assert_equal Locale::Tag::Common, lang.class
    assert_equal "zh", lang.language
    assert_equal "CN", lang.region
    assert_equal "Latn", lang.script
    assert_equal ["variant1"], lang.variants

    assert_equal [Locale::Tag::Common.parse("zh-Latn-CN-variant1"),
                  Locale::Tag::Common.parse("zh-Latn-CN"),
                  Locale::Tag::Common.parse("zh-CN-variant1"),
                  Locale::Tag::Common.parse("zh-CN"),
                  Locale::Tag::Common.parse("zh-Latn-variant1"),
                  Locale::Tag::Common.parse("zh-Latn"),
                  Locale::Tag::Common.parse("zh-variant1"),
                  Locale::Tag::Common.parse("zh"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("zh_CN"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("zh_Latn_CN-variant1"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("zh-Latn-CN-variant1"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("zh-Latn-CN_VARIANT1"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("zh_CN@variant1"), lang.to_posix
  end

  def test_common_lang_region_variants_rfc3066
    # RFC3066 compatible
    lang = Locale::Tag.parse("ja-jp-mac")
    assert_equal Locale::Tag::Common, lang.class
    assert_equal "ja", lang.language
    assert_equal "JP", lang.region
    assert_equal ["mac"], lang.variants
    assert_equal nil, lang.script
    assert_equal [Locale::Tag::Common.parse("ja_JP_mac"),
                  Locale::Tag::Common.parse("ja_JP"),
                  Locale::Tag::Common.parse("ja_JP_mac"),
                  Locale::Tag::Common.parse("ja_JP"),
                  Locale::Tag::Common.parse("ja_mac"),
                  Locale::Tag::Common.parse("ja"),
                  Locale::Tag::Common.parse("ja_mac"),
                  Locale::Tag::Common.parse("ja"),
                 ], lang.candidates

  end

  def test_common_writer
    lang = Locale::Tag.parse("zh-Latn-CN-variant1")
    lang.language = "ja"
    assert_equal "ja_Latn_CN_variant1", lang.to_s
    lang.script = "Hira"
    assert_equal "ja_Hira_CN_variant1", lang.to_s
    lang.region = "JP"
    assert_equal "ja_Hira_JP_variant1", lang.to_s
    lang.variants = ["variant1", "variant2"]
    assert_equal "ja_Hira_JP_variant1_variant2", lang.to_s

    assert_equal [Locale::Tag::Common.parse("ja-Hira-JP-variant1-variant2"),
                  Locale::Tag::Common.parse("ja-Hira-JP"),
                  Locale::Tag::Common.parse("ja-JP-variant1-variant2"),
                  Locale::Tag::Common.parse("ja-JP"),
                  Locale::Tag::Common.parse("ja-Hira-variant1-variant2"),
                  Locale::Tag::Common.parse("ja-Hira"),
                  Locale::Tag::Common.parse("ja-variant1-variant2"),
                  Locale::Tag::Common.parse("ja"),
                 ], lang.candidates

  end

  def test_rfc_lang_script_region_variant_extension
    lang = Locale::Tag.parse("zh-Latn-CN-variant1-a-extend1")
    assert_equal Locale::Tag::Rfc, lang.class
    assert_equal "zh", lang.language
    assert_equal "CN", lang.region
    assert_equal "Latn", lang.script
    assert_equal ["variant1"], lang.variants
    assert_equal ["a-extend1"], lang.extensions
    assert_equal nil, lang.privateuse

    assert_equal [Locale::Tag::Rfc.parse("zh-Latn-CN-variant1"),
                  Locale::Tag::Rfc.parse("zh-Latn-CN"),
                  Locale::Tag::Rfc.parse("zh-CN-variant1"),
                  Locale::Tag::Rfc.parse("zh-CN"),
                  Locale::Tag::Rfc.parse("zh-Latn-variant1"),
                  Locale::Tag::Rfc.parse("zh-Latn"),
                  Locale::Tag::Rfc.parse("zh-variant1"),
                  Locale::Tag::Rfc.parse("zh"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("zh_CN"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("zh-Latn-CN_variant1"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("zh-Latn-CN-variant1-a-extend1"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("zh_Latn_CN_VARIANT1"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("zh_CN@variant1"), lang.to_posix
  end

  def test_rfc_lang_script_region_extensions
    lang = Locale::Tag.parse("ja-Kana-JP-0abc-y-aa-z-bbccdd-b-nnnnnnnn")
    assert_equal Locale::Tag::Rfc, lang.class
    assert_equal "ja", lang.language
    assert_equal "JP", lang.region
    assert_equal "Kana", lang.script
    assert_equal ["0abc"], lang.variants
    assert_equal ["y-aa", "z-bbccdd", "b-nnnnnnnn"], lang.extensions

    assert_equal [Locale::Tag::Rfc.parse("ja-Kana-JP-0abc"),
                  Locale::Tag::Rfc.parse("ja-Kana-JP"),
                  Locale::Tag::Rfc.parse("ja-JP-0abc"),
                  Locale::Tag::Rfc.parse("ja-JP"),
                  Locale::Tag::Rfc.parse("ja-Kana-0abc"),
                  Locale::Tag::Rfc.parse("ja-Kana"),
                  Locale::Tag::Rfc.parse("ja-0abc"),
                  Locale::Tag::Rfc.parse("ja"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("ja_JP"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("ja-Kana-JP-0abc"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("ja-Kana-JP-0abc-y-aa-z-bbccdd-b-nnnnnnnn"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("ja_Kana_JP_0abc"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("ja_JP@0abc"), lang.to_posix
  end

  def test_rfc_lang_region_variants_extensions
    lang = Locale::Tag.parse("ja-JP-0abc-y-aa-z-bbccdd-b-xxxnnnnn")
    assert_equal Locale::Tag::Rfc, lang.class
    assert_equal "ja", lang.language
    assert_equal "JP", lang.region
    assert_equal nil, lang.script
    assert_equal ["0abc"], lang.variants
    assert_equal ["y-aa", "z-bbccdd", "b-xxxnnnnn"], lang.extensions

    assert_equal [Locale::Tag::Rfc.parse("ja-JP-0abc"),
                  Locale::Tag::Rfc.parse("ja-JP"),
                  Locale::Tag::Rfc.parse("ja-JP-0abc"),
                  Locale::Tag::Rfc.parse("ja-JP"),
                  Locale::Tag::Rfc.parse("ja-0abc"),
                  Locale::Tag::Rfc.parse("ja"),
                  Locale::Tag::Rfc.parse("ja-0abc"),
                  Locale::Tag::Rfc.parse("ja"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("ja_JP"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("ja-JP-0abc"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("ja-JP-0abc-y-aa-z-bbccdd-b-xxxnnnnn"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("ja_JP_0abc"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("ja_JP@0abc"), lang.to_posix

  end

  def test_rfc_lang_extensions
    lang = Locale::Tag.parse("ja-y-aa-z-bbccdd-b-xxnnnnn")
    assert_equal Locale::Tag::Rfc, lang.class
    assert_equal "ja", lang.language
    assert_equal nil, lang.region
    assert_equal nil, lang.script
    assert_equal [], lang.variants
    assert_equal ["y-aa", "z-bbccdd", "b-xxnnnnn"], lang.extensions

    assert_equal [Locale::Tag::Rfc.parse("ja"),
                  Locale::Tag::Rfc.parse("ja"),
                  Locale::Tag::Rfc.parse("ja"),
                  Locale::Tag::Rfc.parse("ja"),
                  Locale::Tag::Rfc.parse("ja"),
                  Locale::Tag::Rfc.parse("ja"),
                  Locale::Tag::Rfc.parse("ja"),
                  Locale::Tag::Rfc.parse("ja"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("ja"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("ja"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("ja-y-aa-z-bbccdd-b-xxnnnnn"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("ja"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("ja"), lang.to_posix
  end

  def test_rfc_privateuse
    #Privateuse
    lang = Locale::Tag.parse("zh-Latn-CN-variant1-a-extend1-x-wadegile-private1")
    assert_equal Locale::Tag::Rfc, lang.class
    assert_equal "zh", lang.language
    assert_equal "CN", lang.region
    assert_equal "Latn", lang.script
    assert_equal ["variant1"], lang.variants
    assert_equal ["a-extend1"], lang.extensions
    assert_equal "x-wadegile-private1", lang.privateuse

    assert_equal [Locale::Tag::Rfc.parse("zh-Latn-CN-variant1"),
                  Locale::Tag::Rfc.parse("zh-Latn-CN"),
                  Locale::Tag::Rfc.parse("zh-CN-variant1"),
                  Locale::Tag::Rfc.parse("zh-CN"),
                  Locale::Tag::Rfc.parse("zh-Latn-variant1"),
                  Locale::Tag::Rfc.parse("zh-Latn"),
                  Locale::Tag::Rfc.parse("zh-variant1"),
                  Locale::Tag::Rfc.parse("zh"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("zh_CN"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("zh-Latn-CN_variant1"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("zh-Latn-CN-variant1-a-extend1-x-wadegile-private1"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("zh_Latn_CN_VARIANT1"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("zh_CN@variant1"), lang.to_posix
  end

  def test_rfc_cldr_conversion
    #Privateuse with x-ldml (CLDR conversion)
    lang = Locale::Tag.parse("en-US-x-ldml-POSIX-k-calendar-islamic-k-collatio-traditio-k-colStren-secondar")
    assert_equal Locale::Tag::Rfc, lang.class
    assert_equal "en", lang.language
    assert_equal "US", lang.region
    assert_equal ["POSIX"], lang.variants
    assert_equal ["k-calendar-islamic", "k-collatio-traditio", "k-colStren-secondar"], lang.extensions
    assert_equal "x-ldml-POSIX-k-calendar-islamic-k-collatio-traditio-k-colStren-secondar", lang.privateuse

    assert_equal [Locale::Tag::Rfc.parse("en-US-POSIX"),
                  Locale::Tag::Rfc.parse("en-US"),
                  Locale::Tag::Rfc.parse("en-US-POSIX"),
                  Locale::Tag::Rfc.parse("en-US"),
                  Locale::Tag::Rfc.parse("en-POSIX"),
                  Locale::Tag::Rfc.parse("en"),
                  Locale::Tag::Rfc.parse("en-POSIX"),
                  Locale::Tag::Rfc.parse("en"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("en_US"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("en-US_POSIX"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("en-US-x-ldml-POSIX-k-calendar-islamic-k-collatio-traditio-k-colStren-secondar"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("en_US_POSIX@calendar=islamic;collatio=traditio;colStren=secondar"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("en_US@POSIX"), lang.to_posix

    lang = Locale::Tag.parse("en-US-x-ldml-k-calendar-islamic-k-collatio-traditio-k-colStren-secondar")
    assert_equal Locale::Tag::Rfc, lang.class
    assert_equal "en", lang.language
    assert_equal "US", lang.region
    assert_equal [], lang.variants
    assert_equal ["k-calendar-islamic", "k-collatio-traditio", "k-colStren-secondar"], lang.extensions
    assert_equal "x-ldml-k-calendar-islamic-k-collatio-traditio-k-colStren-secondar", lang.privateuse

    assert_equal [Locale::Tag::Rfc.parse("en-US"),
                  Locale::Tag::Rfc.parse("en-US"),
                  Locale::Tag::Rfc.parse("en-US"),
                  Locale::Tag::Rfc.parse("en-US"),
                  Locale::Tag::Rfc.parse("en"),
                  Locale::Tag::Rfc.parse("en"),
                  Locale::Tag::Rfc.parse("en"),
                  Locale::Tag::Rfc.parse("en"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("en_US"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("en-US"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("en-US-x-ldml-k-calendar-islamic-k-collatio-traditio-k-colStren-secondar"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("en_US@calendar=islamic;collatio=traditio;colStren=secondar"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("en_US"), lang.to_posix
  end

  def test_rfc_writer
    lang = Locale::Tag.parse("zh-Latn-CN-variant1-a-extend1-x-wadegile-private1")
    lang.language = "ja"
    assert_equal "ja-Latn-CN-variant1-a-extend1-x-wadegile-private1", lang.to_s
    lang.script = "Hira"
    assert_equal "ja-Hira-CN-variant1-a-extend1-x-wadegile-private1", lang.to_s
    lang.region = "JP"
    assert_equal "ja-Hira-JP-variant1-a-extend1-x-wadegile-private1", lang.to_s
    lang.variants = ["variant2"]
    assert_equal "ja-Hira-JP-variant2-a-extend1-x-wadegile-private1", lang.to_s
    lang.extensions = ["b-extend2"]
    assert_equal "ja-Hira-JP-variant2-b-extend2-x-wadegile-private1", lang.to_s
    lang.privateuse = "x-foooo"
    assert_equal "ja-Hira-JP-variant2-b-extend2-x-foooo", lang.to_s

    assert_equal [Locale::Tag::Rfc.parse("ja-Hira-JP-variant2"),
                  Locale::Tag::Rfc.parse("ja-Hira-JP"),
                  Locale::Tag::Rfc.parse("ja-JP-variant2"),
                  Locale::Tag::Rfc.parse("ja-JP"),
                  Locale::Tag::Rfc.parse("ja-Hira-variant2"),
                  Locale::Tag::Rfc.parse("ja-Hira"),
                  Locale::Tag::Rfc.parse("ja-variant2"),
                  Locale::Tag::Rfc.parse("ja"),
                 ], lang.candidates
  end

  def test_cldr_lang_extensions
    lang = Locale::Tag.parse("de@collation=phonebook")
    assert_equal Locale::Tag::Cldr, lang.class
    assert_equal "de", lang.language
    assert_equal nil, lang.region
    assert_equal "phonebook", lang.extensions["collation"]

    assert_equal [Locale::Tag::Cldr.parse("de"),
                  Locale::Tag::Cldr.parse("de"),
                  Locale::Tag::Cldr.parse("de"),
                  Locale::Tag::Cldr.parse("de"),
                  Locale::Tag::Cldr.parse("de"),
                  Locale::Tag::Cldr.parse("de"),
                  Locale::Tag::Cldr.parse("de"),
                  Locale::Tag::Cldr.parse("de"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("de"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("de"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("de-k-collatio-phoneboo"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("de@collation=phonebook"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("de"), lang.to_posix
  end

  def test_cldr_lang_script_extension
    lang = Locale::Tag.parse("az_Arab@collation=phonebook")
    assert_equal Locale::Tag::Cldr, lang.class
    assert_equal "az", lang.language
    assert_equal nil, lang.region
    assert_equal "Arab", lang.script
    assert_equal "phonebook", lang.extensions["collation"]

    assert_equal [Locale::Tag::Cldr.parse("az_Arab"),
                  Locale::Tag::Cldr.parse("az_Arab"),
                  Locale::Tag::Cldr.parse("az"),
                  Locale::Tag::Cldr.parse("az"),
                  Locale::Tag::Cldr.parse("az_Arab"),
                  Locale::Tag::Cldr.parse("az_Arab"),
                  Locale::Tag::Cldr.parse("az"),
                  Locale::Tag::Cldr.parse("az"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("az"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("az_Arab"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("az-Arab-k-collatio-phoneboo"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("az_Arab@collation=phonebook"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("az"), lang.to_posix

  end

  def test_cldr_lang_region_extensions
    lang = Locale::Tag.parse("de_DE@collation=phonebook;currency=DDM")
    assert_equal Locale::Tag::Cldr, lang.class
    assert_equal "de", lang.language
    assert_equal "DE", lang.region
    assert_equal "phonebook", lang.extensions["collation"]
    assert_equal "DDM", lang.extensions["currency"]

    assert_equal [Locale::Tag::Cldr.parse("de_DE"),
                  Locale::Tag::Cldr.parse("de_DE"),
                  Locale::Tag::Cldr.parse("de_DE"),
                  Locale::Tag::Cldr.parse("de_DE"),
                  Locale::Tag::Cldr.parse("de"),
                  Locale::Tag::Cldr.parse("de"),
                  Locale::Tag::Cldr.parse("de"),
                  Locale::Tag::Cldr.parse("de"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("de_DE"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("de_DE"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("de-DE-k-collatio-phoneboo-k-currency-DDM"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("de_DE@collation=phonebook;currency=DDM"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("de_DE"), lang.to_posix

    lang = Locale::Tag.parse("en_US@calendar=islamic;collation=traditional;colStrength=secondary")
    assert_equal Locale::Tag::Cldr, lang.class
    assert_equal "en", lang.language
    assert_equal "US", lang.region
    assert_equal [], lang.variants
    assert_equal "islamic", lang.extensions["calendar"]
    assert_equal "traditional", lang.extensions["collation"]
    assert_equal "secondary", lang.extensions["colStrength"]

    assert_equal [Locale::Tag::Cldr.parse("en_US"),
                  Locale::Tag::Cldr.parse("en_US"),
                  Locale::Tag::Cldr.parse("en_US"),
                  Locale::Tag::Cldr.parse("en_US"),
                  Locale::Tag::Cldr.parse("en"),
                  Locale::Tag::Cldr.parse("en"),
                  Locale::Tag::Cldr.parse("en"),
                  Locale::Tag::Cldr.parse("en"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("en_US"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("en_US"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("en-US-k-calendar-islamic-k-collatio-traditio-k-colStren-secondar"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("en_US@calendar=islamic;collation=traditional;colStrength=secondary"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("en_US"), lang.to_posix

  end

  def test_cldr_lang_region_variants_extensions
    lang = Locale::Tag.parse("en_US_POSIX@calendar=islamic;collation=traditional;colStrength=secondary")
    assert_equal Locale::Tag::Cldr, lang.class
    assert_equal "en", lang.language
    assert_equal "US", lang.region
    assert_equal ["POSIX"], lang.variants
    assert_equal "islamic", lang.extensions["calendar"]
    assert_equal "traditional", lang.extensions["collation"]
    assert_equal "secondary", lang.extensions["colStrength"]

    assert_equal [Locale::Tag::Cldr.parse("en_US_POSIX"),
                  Locale::Tag::Cldr.parse("en_US"),
                  Locale::Tag::Cldr.parse("en_US_POSIX"),
                  Locale::Tag::Cldr.parse("en_US"),
                  Locale::Tag::Cldr.parse("en_POSIX"),
                  Locale::Tag::Cldr.parse("en"),
                  Locale::Tag::Cldr.parse("en_POSIX"),
                  Locale::Tag::Cldr.parse("en"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("en_US"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("en_US_POSIX"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("en-US-POSIX-k-calendar-islamic-k-collatio-traditio-k-colStren-secondar"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("en_US_POSIX@calendar=islamic;collation=traditional;colStrength=secondary"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("en_US@POSIX"), lang.to_posix
  end

  def test_cldr_writer
    lang = Locale::Tag.parse("en_US@calendar=islamic;colStrength=secondary")
    lang.language = "ja"
    assert_equal "ja_US@calendar=islamic;colStrength=secondary", lang.to_s
    lang.script = "Hira"
    assert_equal "ja_Hira_US@calendar=islamic;colStrength=secondary", lang.to_s
    lang.region = "JP"
    assert_equal "ja_Hira_JP@calendar=islamic;colStrength=secondary", lang.to_s
    lang.variants = ["POSIX"]
    assert_equal "ja_Hira_JP_POSIX@calendar=islamic;colStrength=secondary", lang.to_s
    lang.extensions = {:collation => "traditional"}
    assert_equal "ja_Hira_JP_POSIX@collation=traditional", lang.to_s

    assert_equal [Locale::Tag::Cldr.parse("ja_Hira_JP_POSIX"),
                  Locale::Tag::Cldr.parse("ja_Hira_JP"),
                  Locale::Tag::Cldr.parse("ja_JP_POSIX"),
                  Locale::Tag::Cldr.parse("ja_JP"),
                  Locale::Tag::Cldr.parse("ja_Hira_POSIX"),
                  Locale::Tag::Cldr.parse("ja_Hira"),
                  Locale::Tag::Cldr.parse("ja_POSIX"),
                  Locale::Tag::Cldr.parse("ja"),
                 ], lang.candidates
  end

  def test_posix_c_and_posix
    lang = Locale::Tag.parse("C")
    assert_equal Locale::Tag::Posix, lang.class
    assert_equal "en", lang.language
    assert_equal "US", lang.region
    assert_equal "C", lang.tag

    assert_equal [Locale::Tag::Posix.parse("en_US"),
                  Locale::Tag::Posix.parse("en_US"),
                  Locale::Tag::Posix.parse("en_US"),
                  Locale::Tag::Posix.parse("en_US"),
                  Locale::Tag::Posix.parse("en"),
                  Locale::Tag::Posix.parse("en"),
                  Locale::Tag::Posix.parse("en"),
                  Locale::Tag::Posix.parse("en"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("en_US"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("en_US"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("en-US"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("en_US"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("en_US"), lang.to_posix

    lang = Locale::Tag.parse("POSIX")
    assert_equal Locale::Tag::Posix, lang.class
    assert_equal "en", lang.language
    assert_equal "US", lang.region
    assert_equal "POSIX", lang.tag

    assert_equal [Locale::Tag::Posix.parse("en_US"),
                  Locale::Tag::Posix.parse("en_US"),
                  Locale::Tag::Posix.parse("en_US"),
                  Locale::Tag::Posix.parse("en_US"),
                  Locale::Tag::Posix.parse("en"),
                  Locale::Tag::Posix.parse("en"),
                  Locale::Tag::Posix.parse("en"),
                  Locale::Tag::Posix.parse("en"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("en_US"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("en_US"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("en-US"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("en_US"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("en_US"), lang.to_posix
  end

  def test_posix_irregular_format
    lang = Locale::Tag.parse("japanese.euc")
    assert_equal Locale::Tag::Posix, lang.class
    assert_equal "japanese", lang.language
    assert_equal nil, lang.region
    assert_equal "euc", lang.charset

    assert_equal [Locale::Tag::Posix.parse("japanese.euc"),
                  Locale::Tag::Posix.parse("japanese.euc"),
                  Locale::Tag::Posix.parse("japanese"),
                  Locale::Tag::Posix.parse("japanese"),
                  Locale::Tag::Posix.parse("japanese.euc"),
                  Locale::Tag::Posix.parse("japanese.euc"),
                  Locale::Tag::Posix.parse("japanese"),
                  Locale::Tag::Posix.parse("japanese"),
                 ], lang.candidates

    lang = Locale::Tag.parse("univ.utf8")
    assert_equal Locale::Tag::Posix, lang.class
    assert_equal "univ", lang.language
    assert_equal nil, lang.region
    assert_equal "utf8", lang.charset

    assert_equal [Locale::Tag::Posix.parse("univ.utf8"),
                  Locale::Tag::Posix.parse("univ.utf8"),
                  Locale::Tag::Posix.parse("univ"),
                  Locale::Tag::Posix.parse("univ"),
                  Locale::Tag::Posix.parse("univ.utf8"),
                  Locale::Tag::Posix.parse("univ.utf8"),
                  Locale::Tag::Posix.parse("univ"),
                  Locale::Tag::Posix.parse("univ"),
                 ], lang.candidates
  end

  def test_posix_lang_charset
    lang = Locale::Tag.parse("es.iso885915")
    assert_equal Locale::Tag::Posix, lang.class
    assert_equal "es", lang.language
    assert_equal nil, lang.region
    assert_equal "iso885915", lang.charset
    assert_equal nil, lang.modifier

    assert_equal [Locale::Tag::Posix.parse("es.iso885915"),
                  Locale::Tag::Posix.parse("es.iso885915"),
                  Locale::Tag::Posix.parse("es"),
                  Locale::Tag::Posix.parse("es"),
                  Locale::Tag::Posix.parse("es.iso885915"),
                  Locale::Tag::Posix.parse("es.iso885915"),
                  Locale::Tag::Posix.parse("es"),
                  Locale::Tag::Posix.parse("es"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("es"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("es"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("es"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("es"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("es.iso885915"), lang.to_posix
  end

  def test_posix_lang_region_charset
    lang = Locale::Tag.parse("es_ES.iso885915")
    assert_equal Locale::Tag::Posix, lang.class
    assert_equal "es", lang.language
    assert_equal "ES", lang.region
    assert_equal "iso885915", lang.charset
    assert_equal nil, lang.modifier

    assert_equal [Locale::Tag::Posix.parse("es_ES.iso885915"),
                  Locale::Tag::Posix.parse("es_ES.iso885915"),
                  Locale::Tag::Posix.parse("es_ES"),
                  Locale::Tag::Posix.parse("es_ES"),
                  Locale::Tag::Posix.parse("es.iso885915"),
                  Locale::Tag::Posix.parse("es.iso885915"),
                  Locale::Tag::Posix.parse("es"),
                  Locale::Tag::Posix.parse("es"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("es_ES"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("es_ES"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("es-ES"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("es_ES"), lang.to_cldr
    assert_equal Locale::Tag::Posix.parse("es_ES.iso885915"), lang.to_posix

    lang = Locale::Tag.parse("ja_JP.UTF-8")
    assert_equal Locale::Tag::Posix, lang.class
    assert_equal "ja", lang.language
    assert_equal "JP", lang.region
    assert_equal "UTF-8", lang.charset

    assert_equal [Locale::Tag::Posix.parse("ja_JP.UTF-8"),
                  Locale::Tag::Posix.parse("ja_JP.UTF-8"),
                  Locale::Tag::Posix.parse("ja_JP"),
                  Locale::Tag::Posix.parse("ja_JP"),
                  Locale::Tag::Posix.parse("ja.UTF-8"),
                  Locale::Tag::Posix.parse("ja.UTF-8"),
                  Locale::Tag::Posix.parse("ja"),
                  Locale::Tag::Posix.parse("ja"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("ja_JP"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("ja_JP"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("ja-JP"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("ja_JP"), lang.to_cldr 
    assert_equal Locale::Tag::Posix.parse("ja_JP.UTF-8"), lang.to_posix
  end

  def test_posix_lang_modifier
    lang = Locale::Tag.parse("es@euro")
    assert_equal Locale::Tag::Posix, lang.class
    assert_equal "es", lang.language
    assert_equal nil, lang.region
    assert_equal nil, lang.charset
    assert_equal "euro", lang.modifier

    assert_equal [Locale::Tag::Posix.parse("es@euro"),
                  Locale::Tag::Posix.parse("es"),
                  Locale::Tag::Posix.parse("es@euro"),
                  Locale::Tag::Posix.parse("es"),
                  Locale::Tag::Posix.parse("es@euro"),
                  Locale::Tag::Posix.parse("es"),
                  Locale::Tag::Posix.parse("es@euro"),
                  Locale::Tag::Posix.parse("es"),
                 ], lang.candidates

    # A modifier is converted to a variant.
    # If the modifier is less than 5 characters, it is not canonical value.
    assert_equal Locale::Tag::Simple.parse("es"), lang.to_simple
    assert_equal Locale::Tag::Common.new("es", nil, nil, ["euro"]), lang.to_common
    assert_equal Locale::Tag::Rfc.new("es", nil, nil, ["euro"]), lang.to_rfc
    assert_equal Locale::Tag::Cldr.new("es", nil, nil, ["euro"]), lang.to_cldr 
    assert_equal Locale::Tag::Posix.parse("es@euro"), lang.to_posix
  end

  def test_posix_lang_region_modifier
    lang = Locale::Tag.parse("es_ES@euro")
    assert_equal Locale::Tag::Posix, lang.class
    assert_equal "es", lang.language
    assert_equal "ES", lang.region
    assert_equal nil, lang.charset
    assert_equal "euro", lang.modifier

    assert_equal [Locale::Tag::Posix.parse("es_ES@euro"),
                  Locale::Tag::Posix.parse("es_ES"),
                  Locale::Tag::Posix.parse("es_ES@euro"),
                  Locale::Tag::Posix.parse("es_ES"),
                  Locale::Tag::Posix.parse("es@euro"),
                  Locale::Tag::Posix.parse("es"),
                  Locale::Tag::Posix.parse("es@euro"),
                  Locale::Tag::Posix.parse("es"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("es_ES"), lang.to_simple
    assert_equal Locale::Tag::Common.new("es", nil, "ES", ["euro"]), lang.to_common
    assert_equal Locale::Tag::Rfc.new("es", nil, "ES", ["euro"]), lang.to_rfc
    assert_equal Locale::Tag::Cldr.new("es", nil, "ES", ["EURO"]), lang.to_cldr 
    assert_equal Locale::Tag::Posix.parse("es_ES@euro"), lang.to_posix

  end

  def test_posix_lang_region_charset_modifier
    lang = Locale::Tag.parse("es_ES.iso885915@euro")
    assert_equal Locale::Tag::Posix, lang.class
    assert_equal "es", lang.language
    assert_equal "ES", lang.region
    assert_equal "iso885915", lang.charset
    assert_equal "euro", lang.modifier

    assert_equal [Locale::Tag::Posix.parse("es_ES.iso885915@euro"),
                  Locale::Tag::Posix.parse("es_ES.iso885915"),
                  Locale::Tag::Posix.parse("es_ES@euro"),
                  Locale::Tag::Posix.parse("es_ES"),
                  Locale::Tag::Posix.parse("es.iso885915@euro"),
                  Locale::Tag::Posix.parse("es.iso885915"),
                  Locale::Tag::Posix.parse("es@euro"),
                  Locale::Tag::Posix.parse("es"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("es_ES"), lang.to_simple
    assert_equal Locale::Tag::Common.new("es", nil, "ES", ["euro"]), lang.to_common
    assert_equal Locale::Tag::Rfc.new("es", nil, "ES", ["euro"]), lang.to_rfc
    assert_equal Locale::Tag::Cldr.new("es", nil, "ES", ["EURO"]), lang.to_cldr 
    assert_equal Locale::Tag::Posix.parse("es_ES.iso885915@euro"), lang.to_posix

  end

  def test_posix_writer
    lang = Locale::Tag.parse("es_ES.iso885915@euro")
    lang.language = "ja"
    assert_equal "ja_ES.iso885915@euro", lang.to_s
    lang.region = "JP"
    assert_equal "ja_JP.iso885915@euro", lang.to_s
    lang.charset = "EUC-JP"
    assert_equal "ja_JP.EUC-JP@euro", lang.to_s
    lang.modifier = "osaka"
    assert_equal "ja_JP.EUC-JP@osaka", lang.to_s
    assert_equal [Locale::Tag::Posix.parse("ja_JP.EUC-JP@osaka"),
                  Locale::Tag::Posix.parse("ja_JP.EUC-JP"),
                  Locale::Tag::Posix.parse("ja_JP@osaka"),
                  Locale::Tag::Posix.parse("ja_JP"),
                  Locale::Tag::Posix.parse("ja.EUC-JP@osaka"),
                  Locale::Tag::Posix.parse("ja.EUC-JP"),
                  Locale::Tag::Posix.parse("ja@osaka"),
                  Locale::Tag::Posix.parse("ja"),
                 ], lang.candidates
  end

  def test_invaild
    lang = Locale::Tag.parse("")
    assert_equal Locale::Tag::Irregular, lang.class
    assert_equal "en", lang.language
    assert_equal nil, lang.region

    assert_equal [Locale::Tag::Irregular.new("en"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("en"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("en"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("en"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("en"), lang.to_cldr 
    assert_equal Locale::Tag::Posix.parse("en"), lang.to_posix

    lang = Locale::Tag.parse(nil)
    assert_equal Locale::Tag::Irregular, lang.class
    assert_equal "en", lang.language
    assert_equal nil, lang.region

    assert_equal [Locale::Tag::Irregular.new("en"),
                 ], lang.candidates

    assert_equal Locale::Tag::Simple.parse("en"), lang.to_simple
    assert_equal Locale::Tag::Common.parse("en"), lang.to_common
    assert_equal Locale::Tag::Rfc.parse("en"), lang.to_rfc
    assert_equal Locale::Tag::Cldr.parse("en"), lang.to_cldr 
    assert_equal Locale::Tag::Posix.parse("en"), lang.to_posix
  end

  def test_raise_error
    assert_raise(RuntimeError){
      Locale::Tag::Simple.new(nil)
    }
  end

end
