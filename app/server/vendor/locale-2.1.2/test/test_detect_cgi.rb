# -*- mode: ruby; coding: utf-8 -*-
#
# Copyright (C) 2012  Kouhei Sutou <kou@clear-code.com>
# Copyright (C) 2009-2010  Masao Mutoh
#
# License: Ruby's or LGPL
#
# This library is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

require 'cgi'
require 'locale'
require 'test/unit'

class CustomizableCGI < CGI
  def initialize(options={})
    yield(self)
    super(options)
  end
end

class TestDetectCGI < Test::Unit::TestCase
  def setup_cgi(env={})
    Locale.init(:driver => :cgi)
    cgi = CustomizableCGI.new do |_cgi|
      default_env = {
        "REQUEST_METHOD" => "GET",
      }
      env = default_env.merge(env)
      stub(_cgi).env_table {env}
    end
    Locale.cgi = cgi
    Locale.clear_all
  end

  def test_query_string
    #query string
    setup_cgi("QUERY_STRING" => "lang=ja_JP")
    lang = Locale.current[0]
    assert_equal(Locale::Tag::Simple, lang.class)
    assert_equal("ja_JP", lang.to_s)

    setup_cgi("QUERY_STRING" => "lang=ja-jp")
    lang = Locale.current[0]
    assert_equal(Locale::Tag::Simple, lang.class)
    assert_equal("ja_JP", lang.to_s)
    assert_equal("ja-JP", lang.to_rfc.to_s)
    setup_cgi("QUERY_STRING" => "lang=ja-jp")
    assert_equal("ja_JP", lang.to_s)
    assert_equal("ja-JP", lang.to_rfc.to_s)

  end

  def test_cookie
    #cockie
    setup_cgi("HTTP_COOKIE" => "lang=en-us")
    assert_equal("en_US", Locale.current.to_s)
  end

  def test_accept_language
    setup_cgi("HTTP_ACCEPT_LANGUAGE" => "",
              "HTTP_ACCEPT_CHARSET" => "")
    lang = Locale.current[0]
    assert_equal(Locale::Tag::Simple, lang.class)
    assert_equal("en", lang.to_s)
    assert_equal("en", lang.to_rfc.to_s)

    setup_cgi("HTTP_ACCEPT_LANGUAGE" => "ja,en-us;q=0.7,en;q=0.3")
    lang1, lang2, lang3 = Locale.current
    assert_equal("ja", lang1.to_rfc.to_s)
    assert_equal("en-US", lang2.to_rfc.to_s)
    assert_equal("en", lang3.to_rfc.to_s)

    setup_cgi("HTTP_ACCEPT_LANGUAGE" => "en-us,ja;q=0.7,en;q=0.3")
    lang1, lang2, lang3 = Locale.current
    assert_equal("en-US", lang1.to_rfc.to_s)
    assert_equal("ja", lang2.to_rfc.to_s)
    assert_equal("en", lang3.to_rfc.to_s)

    setup_cgi("HTTP_ACCEPT_LANGUAGE" => "en")
    lang = Locale.current[0]
    assert_equal("en", lang.to_rfc.to_s)
  end

  def test_accept_charset
    #accept charset
    setup_cgi("HTTP_ACCEPT_CHARSET" => "Shift_JIS")
    assert_equal("Shift_JIS", Locale.charset)

    setup_cgi("HTTP_ACCEPT_CHARSET" => "EUC-JP,*,utf-8")
    assert_equal("EUC-JP", Locale.charset)

    setup_cgi("HTTP_ACCEPT_CHARSET" => "*")
    assert_equal("UTF-8", Locale.charset)

    setup_cgi("HTTP_ACCEPT_CHARSET" => "")
    assert_equal("UTF-8", Locale.charset)
  end

  def test_default
    Locale.set_default(nil)
    Locale.set_default("ja-JP")
    setup_cgi("HTTP_ACCEPT_LANGUAGE" => "",
              "HTTP_ACCEPT_CHARSET" => "")
    assert_equal("ja-JP", Locale.default.to_rfc.to_s)
    assert_equal("ja-JP", Locale.current.to_rfc.to_s)
    Locale.set_default(nil)
  end

  def common(*ary)
    ary.map{|v| Locale::Tag::Common.parse(v)}
  end

  def rfc(*ary)
    ary.map{|v| Locale::Tag::Rfc.parse(v)}
  end

  def cldr(*ary)
    ary.map{|v| Locale::Tag::Cldr.parse(v)}
  end

  def simple(*ary)
    ary.map{|v| Locale::Tag::Simple.parse(v)}
  end

  def test_candidates

    setup_cgi("HTTP_ACCEPT_LANGUAGE" => "fr-fr,zh_CN;q=0.7,zh_TW;q=0.2,ja_JP;q=0.1")

    assert_equal common("fr-FR", "zh-CN", "zh-TW", "ja-JP", 
                        "fr", "zh", "ja", "en"), Locale.candidates
    
    assert_equal rfc("fr-FR", "zh-CN", "zh-TW", "ja-JP", "fr", 
                     "zh", "ja", "en"), Locale.candidates(:type => :rfc)

    assert_equal cldr("fr_FR", "zh_CN", "zh_TW", "ja_JP", "fr", 
                      "zh", "ja", "en"), Locale.candidates(:type => :cldr)

    assert_equal simple("fr-FR", "zh-CN", "zh-TW", "ja-JP",  
                        "fr", "zh", "ja", "en"), Locale.candidates(:type => :simple)

    taglist = Locale.candidates(:type => :rfc)
    assert_equal Locale::TagList, taglist.class
    assert_equal "fr", taglist.language
    assert_equal "FR", taglist.region

  end

  def test_candidates_with_supported_language_tags
    setup_cgi("HTTP_ACCEPT_LANGUAGE" => "fr-fr,zh_CN;q=0.7,zh_TW;q=0.2,ja_JP;q=0.1")

    assert_equal common("fr_FR", "zh", "ja"), Locale.candidates(:type => :common, 
                                                                :supported_language_tags => ["fr_FR", "ja", "zh"])

    assert_equal simple("fr-FR", "zh", "ja"), Locale.candidates(:type => :simple, 
                                                                :supported_language_tags => ["fr-FR", "ja", "zh"])
    #supported_language_tags includes "pt" as not in HTTP_ACCEPT_LANGUAGE
    assert_equal simple("fr-FR", "zh", "ja"), 
    Locale.candidates(:type => :simple, 
                      :supported_language_tags => ["fr-FR", "ja", "zh", "pt"])

  end

  def test_candidates_with_default
    setup_cgi("HTTP_ACCEPT_LANGUAGE" => "fr-fr,zh_CN;q=0.7,zh_TW;q=0.2,ja_JP;q=0.1")

    Locale.default = "zh_TW"
    assert_equal simple("fr-FR", "zh", "ja"), 
    Locale.candidates(:type => :simple, 
                      :supported_language_tags => ["fr-FR", "ja", "zh", "pt"])

    Locale.default = "pt"
    assert_equal simple("fr-FR", "zh", "ja", "pt"), 
    Locale.candidates(:type => :simple, 
                      :supported_language_tags => ["fr-FR", "ja", "zh", "pt"])

    # default value is selected even if default is not in supported_language_tags.
    assert_equal simple("pt"), Locale.candidates(:type => :simple, 
                                                 :supported_language_tags => ["aa"])
    Locale.default = "en"
  end


  def test_candidates_with_app_language_tags
    Locale.set_app_language_tags("fr-FR", "ja")

    setup_cgi("HTTP_ACCEPT_LANGUAGE" => "fr-fr,zh_CN;q=0.7,zh_TW;q=0.2,ja_JP;q=0.1")

    assert_equal common("fr-FR", "ja"), Locale.candidates

    # default value is selected if default is not in app_language_tags.
    Locale.set_app_language_tags("no", "pt")
    Locale.default = "zh"
    assert_equal common("zh"), Locale.candidates

    Locale.default = "en"
    Locale.set_app_language_tags(nil)
  end

  def test_request
    Locale.set_request(["ja"], [""], "", "")
    assert_equal common("ja", "en"), Locale.candidates

    Locale.set_request(["en"], [""], "", "")
    assert_equal common("en"), Locale.candidates #Cache should be cleared.
  end
end
