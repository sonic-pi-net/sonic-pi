# -*- coding: utf-8 -*-
#
# Copyright (C) 2012  Kouhei Sutou <kou@clear-code.com>
# Copyright (C) 2012  Hleb Valoshka
# Copyright (C) 2008  Masao Mutoh
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

require 'locale/tag'
require 'locale/taglist'
require "locale/driver"

module Locale
  module Driver
    # Locale::Driver::Env module.
    # Detect the user locales and the charset.
    # All drivers(except CGI) refer environment variables first and use it
    # as the locale if it's defined.
    # This is a low-level module. Application shouldn't use this directly.
    module Env
      module_function

      # Gets the locale from environment variable.
      # Priority order except charset is LC_ALL > LC_MESSAGES > LANG.
      # Priority order for charset is LC_ALL > LC_CTYPE > LANG.
      # Returns: the locale as Locale::Tag::Posix.
      def locale
        lc_all = Private.parse(ENV["LC_ALL"])
        return lc_all if lc_all

        lc_messages = Private.parse(ENV["LC_MESSAGES"])
        lang = Private.parse(ENV["LANG"])

        tag = lc_messages || lang
        return nil if tag.nil?

        lc_ctype = Private.parse(ENV["LC_CTYPE"])
        tag.charset = lc_ctype.charset if lc_ctype

        tag
      end

      # Gets the locales from environment variables. (LANGUAGE > LC_ALL > LC_MESSAGES > LANG)
      # * Returns: an Array of the locale as Locale::Tag::Posix or nil.
      def locales
        return nil if (ENV["LC_ALL"] || ENV["LC_MESSAGES"] || ENV["LANG"]) == "C"
        locales = ENV["LANGUAGE"]
        if (locales != nil and locales.size > 0)
          locs = locales.split(/:/).collect{|v| Locale::Tag::Posix.parse(v)}.compact
          if locs.size > 0
            return Locale::TagList.new(locs)
          end
        elsif (loc = locale)
          return Locale::TagList.new([loc])
        end
        nil
      end

      # Gets the charset from environment variables
      # (LC_ALL > LC_CTYPE > LANG) or return nil.
      # * Returns: the system charset.
      def charset  # :nodoc:
        [ENV["LC_ALL"], ENV["LC_CTYPE"], ENV["LANG"]].each do |env|
          tag = Private.parse(env)
          next if tag.nil?
          return tag.charset
        end
        nil
      end

      module Private
        module_function
        def parse(env_value)
          return nil if env_value.nil?
          return nil if env_value.empty?
          Locale::Tag::Posix.parse(env_value)
        end
      end
    end

    MODULES[:env] = Env
  end
end

