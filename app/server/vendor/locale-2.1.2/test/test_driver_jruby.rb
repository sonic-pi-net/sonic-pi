# -*- coding: utf-8 -*-
#
# Copyright (C) 2012  Kouhei Sutou <kou@clear-code.com>
# Copyright (C) 2012  Hleb Valoshka
# Copyright (C) 2009  Masao Mutoh
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

begin
  require 'locale/driver/jruby'
  require 'test/unit'
  class TestDiverJRuby < Test::Unit::TestCase

    def setup
      ENV["LC_ALL"] = nil
      ENV["LC_CTYPE"] = nil
      ENV["LANG"] = nil
      ENV["LANGUAGE"] = nil
    end

    def set_locale(tag)
      locale = java.util.Locale.new(tag.language, tag.region,
                                    tag.variants.join("_"))
      java.util.Locale.setDefault(locale)
    end

    def test_charset
      # Depends on system value when jvm is started.
    end

    def test_locales
      tag = Locale::Tag::Common.parse("ja-JP")
      set_locale(tag)
      assert_equal [tag], Locale::Driver::JRuby.locales
    end

    def test_locales_with_env
      ENV["LC_ALL"] = "ja_JP.EUC-JP"
      assert_equal Locale::Tag::Posix.parse("ja_JP.EUC-JP"), Locale::Driver::JRuby.locales[0]
      assert_equal "EUC-JP", Locale::Driver::JRuby.charset

      ENV["LC_ALL"] = "ja_JP"
      assert_equal Locale::Tag::Posix.parse("ja_JP"), Locale::Driver::JRuby.locales[0]
  
      ENV["LC_ALL"] = "C"
      assert_equal Locale::Tag::Posix.parse("C"), Locale::Driver::JRuby.locales[0]
    end
  end

rescue LoadError
  puts "jruby test was skipped."
end
