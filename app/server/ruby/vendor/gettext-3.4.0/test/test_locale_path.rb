# -*- coding: utf-8 -*-
#
# Copyright (C) 2013  Haruka Yoshihara <yoshihara@clear-code.com>
# Copyright (C) 2012-2013  Kouhei Sutou <kou@clear-code.com>
# Copyright (C) 2010  masone (Christian Felder) <ema@rh-productions.ch>
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

require 'fixtures/simple'

class TestLocalePath < Test::Unit::TestCase
  def setup
    GetText.locale = "ja_JP.eucJP"
  end

  def teardown
    GetText.locale = nil
  end

  def test_locale_path
    test = Simple.new
    assert_equal("japanese", test.test)
    prefix = GetText::LocalePath::CONFIG_PREFIX
    default_locale_dirs = [
      "./locale/%{lang}/LC_MESSAGES/%{name}.mo",
      "./locale/%{lang}/%{name}.mo",
      "#{RbConfig::CONFIG['datadir']}/locale/%{lang}/LC_MESSAGES/%{name}.mo",
      "#{RbConfig::CONFIG['datadir'].gsub(/\/local/, "")}/locale/%{lang}/LC_MESSAGES/%{name}.mo",
      "#{prefix}/share/locale/%{lang}/LC_MESSAGES/%{name}.mo",
      "#{prefix}/local/share/locale/%{lang}/LC_MESSAGES/%{name}.mo"
    ].uniq
    assert_equal(default_locale_dirs, GetText::LocalePath::DEFAULT_RULES)
    new_path = "/foo/%{lang}/%{name}.mo"
    GetText::LocalePath.add_default_rule(new_path)
    assert_equal([new_path] + default_locale_dirs, GetText::LocalePath::DEFAULT_RULES)
  end

  def test_initialize_with_topdir
    testdir = File.dirname(File.expand_path(__FILE__))
    path = GetText::LocalePath.new("test1", "#{testdir}/locale")
    assert_equal({
                   "ja"      => "#{testdir}/locale/ja/LC_MESSAGES/test1.mo",
                   "fr"      => "#{testdir}/locale/fr/LC_MESSAGES/test1.mo",
                   "zh_Hant" => "#{testdir}/locale/zh_Hant/LC_MESSAGES/test1.mo"
                 },
                 path.locale_paths)
    assert_equal("#{testdir}/locale/ja/LC_MESSAGES/test1.mo",
                 path.current_path(Locale::Tag.parse("ja")))
    assert_equal("#{testdir}/locale/ja/LC_MESSAGES/test1.mo",
                 path.current_path(Locale::Tag.parse("ja-JP")))
    assert_equal("#{testdir}/locale/ja/LC_MESSAGES/test1.mo",
                 path.current_path(Locale::Tag.parse("ja_JP.UTF-8")))
    assert_equal(nil,
                 path.current_path(Locale::Tag.parse("en")))
    assert_equal("#{testdir}/locale/zh_Hant/LC_MESSAGES/test1.mo",
                 path.current_path(Locale::Tag.parse("zh-Hant")))
  end

  def test_supported_locales
    testdir = File.dirname(File.expand_path(__FILE__))
    path = GetText::LocalePath.new("test1", "#{testdir}/locale")
    assert_equal ["fr", "ja", "zh_Hant"], path.supported_locales

    path = GetText::LocalePath.new("plural", "#{testdir}/locale")
    assert_equal ["cr", "da", "fr", "ir", "ja", "la", "li", "po", "sl"], path.supported_locales

    path = GetText::LocalePath.new("nodomain", "#{testdir}/locale")
    assert_equal [], path.supported_locales
  end

  def test_env_GETTEXT_PATH
    topdir = File.join(File.dirname(File.expand_path(__FILE__)), "../samples")
    path1 = File.join(topdir, "locale")
    path2 = File.join(topdir, "cgi", "locale")

    ENV["GETTEXT_PATH"] = path1
    default_path_rules = GetText::LocalePath.default_path_rules
    assert_match(Regexp.compile(path1), default_path_rules[0])

    ENV["GETTEXT_PATH"] = "#{path1},#{path2}"
    default_path_rules = GetText::LocalePath.default_path_rules
    assert_match(Regexp.compile(path1), default_path_rules[0])
    assert_match(Regexp.compile(path2), default_path_rules[1])
  end

  class TestDefaultPathRules < self
    def test_load_path_untached
      $LOAD_PATH.unshift("./lib")
      GetText::LocalePath.default_path_rules
      assert_equal($LOAD_PATH[0], "./lib")
    end
  end
end
