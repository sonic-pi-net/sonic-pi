# Copyright (C) 2020  Sutou Kouhei <kou@clear-code.com>
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

require "gettext/tools/parser/gtk_builder_ui_definitions"

class TestGtkBuilderUIDefinitionsParser < Test::Unit::TestCase
  include Helper::Parser

  def parse(file)
    GetText::GtkBuilderUIDefinitionsParser.parse(fixture_path(file))
  end

  def test_ui
    assert_parse([
                   {
                     :msgid => "label with context",
                     :msgctxt => "context",
                     :references => ["gtk_builder_ui_definitions.ui:19"],
                   },
                   {
                     :msgid => "label simple",
                     :references => ["gtk_builder_ui_definitions.ui:31"],
                   },
                   {
                     :msgid => "multiple\n" + "lines\n" + "label",
                     :references => ["gtk_builder_ui_definitions.ui:55"],
                   },
                 ],
                 "gtk_builder_ui_definitions.ui")
  end

  class TestDetect < self
    def target?(file)
      GetText::GtkBuilderUIDefinitionsParser.target?(fixture_path(file))
    end

    def test_ui
      assert do
        target?("gtk_builder_ui_definitions.ui")
      end
    end

    def test_glade_3
      assert do
        target?("glade/3.glade")
      end
    end
  end
end
