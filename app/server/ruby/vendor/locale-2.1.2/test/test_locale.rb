# Copyright (C) 2012  Kouhei Sutou <kou@clear-code.com>
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

class TestLocale < Test::Unit::TestCase
  class TestCurrent < self
    def test_set_again
      Locale.current = "ja_JP"
      Locale.current = Locale.current
      assert_equal([Locale::Tag::Simple.new("ja", "JP")], Locale.current)
    end
  end

  class TestDefault < self
    def test_set_tag_list
      default_tag_list = Locale::TagList.new
      default_tag_list << Locale::Tag::Simple.new("ja", "JP")
      Locale.default = default_tag_list
      assert_equal(Locale::Tag::Simple.new("ja", "JP"), Locale.default)
    end
  end
end
