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

require "locale/driver"

module Locale
  # Locale::Driver module for CGI.
  # Detect the user locales and the charset from CGI parameters.
  # This is a low-level class. Application shouldn't use this directly.
  module Driver
    module CGI
      $stderr.puts self.name + " is loaded." if $DEBUG

      module_function
      # Gets required locales from CGI parameters. (Based on RFC2616)
      #
      # Returns: An Array of Locale::Tag's subclasses
      #          (QUERY_STRING "lang" > COOKIE "lang" > HTTP_ACCEPT_LANGUAGE > "en")
      # 
      def locales
        req = Thread.current[:current_request]
        return nil unless req

        locales = []

        # QUERY_STRING "lang"
        if langs = req[:query_langs]
          langs.each do |lang|
            locales << Locale::Tag.parse(lang)
          end
        end

        unless locales.size > 0
          # COOKIE "lang"
          if langs = req[:cookie_langs]
            langs.each do |lang|
              locales << Locale::Tag.parse(lang) if lang.size > 0
            end
          end
        end

        unless locales.size > 0
          # HTTP_ACCEPT_LANGUAGE
          if lang = req[:accept_language] and lang.size > 0
            # 10.0 is for ruby-1.8.6 which have the bug of str.to_f. 
            # Normally, this should be 1.0.
            locales += lang.gsub(/\s/, "").split(/,/).map{|v| v.split(";q=")}.map{|j| [j[0], j[1] ? j[1].to_f : 10.0]}.sort{|a,b| -(a[1] <=> b[1])}.map{|v| Locale::Tag.parse(v[0])}
          end
        end

        locales.size > 0 ? Locale::TagList.new(locales.uniq) : nil
      end

      # Gets the charset from CGI parameters. (Based on RFC2616)
      #  * Returns: the charset (HTTP_ACCEPT_CHARSET or nil).
      def charset
        req = Thread.current[:current_request]
        return nil unless req

        charsets = req[:accept_charset]
        if charsets and charsets.size > 0
          num = charsets.index(',')
          charset = num ? charsets[0, num] : charsets
          charset = nil if charset == "*"
        else
          charset = nil
        end
        charset
      end

      # Set a request.
      # 
      # * query_langs: An Array of QUERY_STRING value "lang".
      # * cookie_langs: An Array of cookie value "lang".
      # * accept_language: The value of HTTP_ACCEPT_LANGUAGE
      # * accept_charset: The value of HTTP_ACCEPT_CHARSET
      def set_request(query_langs, cookie_langs, accept_language, accept_charset)
        Locale.clear
        Thread.current[:current_request] = {
          :query_langs => query_langs, 
          :cookie_langs => cookie_langs, 
          :accept_language => accept_language,
          :accept_charset => accept_charset
        }
        self
      end

      # Clear the current request.
      def clear_current_request
        Thread.current[:current_request] = nil
      end
    end

    MODULES[:cgi] = CGI
  end

  
  module_function
  # Sets a request values for lang/charset.
  #
  # * query_langs: An Array of QUERY_STRING value "lang".
  # * cookie_langs: An Array of cookie value "lang".
  # * accept_language: The value of HTTP_ACCEPT_LANGUAGE
  # * accept_charset: The value of HTTP_ACCEPT_CHARSET
  def set_request(query_langs, cookie_langs, accept_language, accept_charset)
    driver_module.set_request(query_langs, cookie_langs, accept_language, accept_charset)
    self
  end

  # Sets a CGI object. This is the convenient function of set_request().
  #
  # This method is appeared when Locale.init(:driver => :cgi) is called.
  #
  # * cgi: CGI object
  # * Returns: self
  def set_cgi(cgi)
    set_request(cgi.params["lang"], cgi.cookies["lang"],
                cgi.accept_language, cgi.accept_charset)
    self
  end
  
  # Sets a CGI object.This is the convenient function of set_request().
  #
  # This method is appeared when Locale.init(:driver => :cgi) is called.
  #
  # * cgi: CGI object
  # * Returns: cgi 
  def cgi=(cgi)
    set_cgi(cgi)
    cgi
  end
end
