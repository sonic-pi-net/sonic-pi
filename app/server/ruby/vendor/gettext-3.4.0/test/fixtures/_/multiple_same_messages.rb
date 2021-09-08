# -*- coding: utf-8 -*-
#
# Copyright (C) 2013-2020  Sutou Kouhei <kou@clear-code.com>
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
    class MultipleSameMessages
      include GetText

      bindtextdomain("_", :path => Helper::Path.locale_path)

      def message
        _("multiple same messages")
      end

      def same_message
        _("multiple same messages")
      end
    end
  end
end
