# -*- coding: utf-8 -*-
#
# Copyright (C) 2013  Kouhei Sutou <kou@clear-code.com>
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

require "gettext/text_domain"

class TestTextDomain < Test::Unit::TestCase
  class TestNomralizeCharset < self
    def test_utf8
      assert_equal("UTF-8", normalize_charset("utf8"))
    end

    def test_valid_charset
      assert_equal("utf-8", normalize_charset("utf-8"))
    end

    private
    def normalize_charset(charset)
      text_domain = GetText::TextDomain.new("hello")
      text_domain.send(:normalize_charset, charset)
    end
  end
end
