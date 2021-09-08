# Copyright (C) 2009-2010  Masao Mutoh
# Copyright (C) 2021  Sutou Kouhei <kou@clear-code.com>
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

require "gettext/tools/parser/glade"

class TestGladeParser < Test::Unit::TestCase
  include Helper::Parser

  def parse(file)
    GetText::GladeParser.parse(fixture_path(file))
  end

  def test_2
    assert_parse([
                   {
                     msgid: "window1",
                     references: ["glade/2.glade:8"],
                   },
                   {
                     msgid: "normal text",
                     references: ["glade/2.glade:29"],
                   },
                   {
                     msgid: "1st line\n2nd line\n3rd line",
                     references: ["glade/2.glade:50"],
                   },
                   {
                     msgid:
                       "<span color=\"red\" " +
                       "weight=\"bold\" " +
                       "size=\"large\">markup </span>",
                     references: ["glade/2.glade:73"],
                   },
                   {
                     msgid:
                       "<span color=\"red\">1st line markup </span>\n" +
                       "<span color=\"blue\">2nd line markup</span>",
                     references: ["glade/2.glade:94"],
                   },
                   {
                     msgid:
                       "<span>" +
                       "&quot;markup&quot; with &lt;escaped strings&gt;" +
                       "</span>",
                    references: ["glade/2.glade:116"],
                   },
                   {
                     msgid: "duplicated",
                     references: [
                       "glade/2.glade:137",
                       "glade/2.glade:158",
                     ],
                   },
                 ],
                 "glade/2.glade")
  end


  class TestDetect < self
    def target?(file)
      GetText::GladeParser.target?(fixture_path(file))
    end

    def test_2
      assert do
        target?("glade/2.glade")
      end
    end

    def test_3
      assert do
        not target?("glade/3.glade")
      end
    end
  end
end
