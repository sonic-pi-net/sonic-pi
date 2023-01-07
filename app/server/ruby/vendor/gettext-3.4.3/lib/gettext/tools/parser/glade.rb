# Copyright (C) 2004,2005  Masao Mutoh
# Copyright (C) 2013       Kouhei Sutou <kou@clear-code.com>
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

require 'cgi'
require 'gettext'

module GetText
  class GladeParser
    extend GetText

    bindtextdomain("gettext")

    class << self
      XML_RE = /<\?xml/
      GLADE_RE = /glade-2.0.dtd/

      def target?(file) # :nodoc:
        return false unless File.extname(file) == ".glade"
        data = File.read(file)
        return false unless data.include?("<?xml")
        return false unless data.include?("glade-2.0.dtd")
        true
      end

      def parse(path, options={})
        parser = new(path, options)
        parser.parse
      end
    end

    TARGET1 = /<property.*translatable="yes">(.*)/
    TARGET2 = /(.*)<\/property>/

    def initialize(path, options={})
      @path = path
      @options = options
    end

    def parse # :nodoc:
      File.open(@path) do |file|
        parse_source(file)
      end
    end

    private
    def parse_source(input) # :nodoc:
      po = []
      target = false
      start_line_no = nil
      val = nil

      input.each_line.with_index do |line, i|
        if TARGET1 =~ line
          start_line_no = i + 1
          val = $1 + "\n"
          target = true
          if TARGET2 =~ $1
            val = $1
            add_po_entry(po, val, start_line_no)
            val = nil
            target = false
          end
        elsif target
          if TARGET2 =~ line
            val << $1
            add_po_entry(po, val, start_line_no)
            val = nil
            target = false
          else
            val << line
          end
        end
      end
      po
    end

    def add_po_entry(po, value, line_no) # :nodoc:
      return if value.empty?
      value = CGI.unescapeHTML(value)
      value = value.gsub(/\n/, "\n")
      po_entry = po.find do |entry|
        entry.msgid == value
      end
      if po_entry.nil?
        po_entry = POEntry.new(:normal)
        po_entry.msgid = value
        po << po_entry
      end
      po_entry.references << "#{@path}:#{line_no}"
    end
  end
end
