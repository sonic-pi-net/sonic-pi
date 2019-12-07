# -*- coding: utf-8 -*-
#
# Copyright (C) 2012  Kouhei Sutou <kou@clear-code.com>
# Copyright (C) 2007-2008 Masao Mutoh
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

require 'java'

require "locale/driver/env"

module Locale
  module Driver
    # Locale::Driver::JRuby module for JRuby
    # Detect the user locales and the charset.
    # This is a low-level class. Application shouldn't use this directly.
    module JRuby
      $stderr.puts self.name + " is loaded." if $DEBUG

      module_function
      def locales  #:nodoc:
        locales = ::Locale::Driver::Env.locales
        unless locales
          locale = java.util.Locale.getDefault
          variant = locale.getVariant 
          variants = []
          if variant != nil and variant.size > 0
            variants = [variant]
          end
          locales = TagList.new([Locale::Tag::Common.new(locale.getLanguage, nil,
                                                         locale.getCountry, 
                                                         variants)])
        end
        locales
      end

      def charset #:nodoc:
        charset = ::Locale::Driver::Env.charset
        unless charset
          charset = java.nio.charset.Charset.defaultCharset.name
        end
        charset
      end
    end

    MODULES[:jruby] = JRuby
  end
end
