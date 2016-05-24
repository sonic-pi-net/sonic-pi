# -*- coding: utf-8 -*-
#
# Copyright (C) 2012  Kouhei Sutou <kou@clear-code.com>
# Copyright (C) 2002-2008  Masao Mutoh
#
# Original: Ruby-GetText-Package-1.92.0.
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

require "locale/driver/env"

module Locale 
  # Locale::Driver::Posix module for Posix OS (Unix)
  # Detect the user locales and the charset.
  # This is a low-level class. Application shouldn't use this directly.
  module Driver
    module Posix
      $stderr.puts self.name + " is loaded." if $DEBUG

      module_function
      # Gets the locales from environment variables. (LANGUAGE > LC_ALL > LC_MESSAGES > LANG)
      # Only LANGUAGE accept plural languages such as "nl_BE;
      # * Returns: an Array of the locale as Locale::Tag::Posix or nil.
      def locales
        ::Locale::Driver::Env.locales
      end

      # Gets the charset from environment variable or the result of
      # "locale charmap" or nil.
      # * Returns: the system charset.
      def charset
        charset = ::Locale::Driver::Env.charset
        unless charset
          charset = `locale charmap`.strip
          unless $? && $?.success?
            charset = nil
          end
        end
        charset
      end
    end

    MODULES[:posix] = Posix
  end
end

