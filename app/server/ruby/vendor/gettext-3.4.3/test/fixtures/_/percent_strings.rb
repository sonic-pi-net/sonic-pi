# Copyright (C) 2020  Kitaiti Makoto <KitaitiMakoto@gmail.com>
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

module Fixtures
  module Method_
    class PercentStrings
      include GetText

      bindtextdomain("_", :path => Helper::Path.locale_path)

      def symbol_array
        _(%i(hello world))
      end

      def in_symbol_array
        %I(before#{_("in_symbol_array")}after)
      end

      def symbol
        _(%s(hello world))
      end

      def string
        _(%(hello world))
      end

      def string_array
        _(%w(hello world))
      end

      def in_string_array
        %W(before#{_("in_string_array")}after)
      end

      def execute
        _(%x(echo hello world))
      end
    end
  end
end
