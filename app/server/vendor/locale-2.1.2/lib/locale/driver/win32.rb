# -*- coding: utf-8 -*-
#
# Copyright (C) 2012  Kouhei Sutou <kou@clear-code.com>
# Copyright (C) 2002-2010  Masao Mutoh
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
require "locale/driver/win32_table"

require "fiddle/import"

module Locale
  # Locale::Driver::Win32 module for win32.
  # Detect the user locales and the charset.
  # This is a low-level class. Application shouldn't use this directly.
  module Driver
    module Win32
      module Kernel32
        extend Fiddle::Importer
        dlload "kernel32.dll"
        extern "int GetThreadLocale()"
      end

      include Win32Table

      $stderr.puts self.name + " is loaded." if $DEBUG

      @@current_locale_id = nil

      module_function

      # Gets the Win32 charset of the locale. 
      def charset
        charset = ::Locale::Driver::Env.charset
        unless charset
          if locales
            tag = locales[0].to_rfc.to_s
            loc = LocaleTable.find{|v| v[1] == tag}
            loc = LocaleTable.find{|v| v[1] =~ /^#{locales[0].language}/} unless loc
            charset = loc ? loc[2] : nil
          else
            charset = "CP1252"
          end
        end
        charset
      end

      def thread_locale_id  #:nodoc:
        if @@current_locale_id
          @@current_locale_id
        else
          Kernel32.GetThreadLocale
        end
      end

      def set_thread_locale_id(lcid)  #:nodoc:
        # for testing.
        @@current_locale_id = lcid
      end

      def locales  #:nodoc:
        locales = ::Locale::Driver::Env.locales
        unless locales
          lang = LocaleTable.assoc(thread_locale_id)
          if lang
            ret = Locale::Tag::Common.parse(lang[1])
            locales = Locale::TagList.new([ret])
          else
            locales = nil
          end
        end
        locales
      end
    end

    MODULES[:win32] = Win32
  end
end

