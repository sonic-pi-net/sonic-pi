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

require "English"
require "cgi"
require "strscan"

require "gettext/po_entry"

module GetText
  class GtkBuilderUIDefinitionsParser
    @config = {
      :extnames => [".ui", ".glade"]
    }

    class << self
      # Sets some preferences to parse GtkBuilder UI definitions files.
      # * config: a Hash of the config. It can takes some values below:
      #   * :extnames: An Array of target files extension.
      #     Default is [".ui", ".glade"].
      def init(config)
        config.each do |k, v|
          @config[k] = v
        end
      end

      def target?(file) # :nodoc:
        @config[:extnames].each do |extname|
          next unless File.extname(file) == extname
          next unless File.read(file).include?("<interface>")
          return true
        end
        false
      end

      def parse(path, options={})
        parser = new(path, options)
        parser.parse
      end
    end

    def initialize(path, options={})
      @path = path
      @options = options
    end

    def parse # :nodoc:
      File.open(@path) do |file|
        po = []
        start_line_no = nil
        property = nil
        file.each_line do |line|
          case line
          when /<property/
            property = $POSTMATCH
            start_line_no = file.lineno
            if /<\/property>/ =~ property
              property << $PREMATCH
              add_po_entry(po, property, start_line_no)
              property = nil
            end
          when /<\/property>/
            property << $PREMATCH
            add_po_entry(po, property, start_line_no)
            property = nil
          else
            property << line if property
          end
        end
        po
      end
    end

    private
    def add_po_entry(po, property, line_no)
      raw_attributes, raw_data_and_close_tag = property.split(">", 2)
      raw_data, _close_tag = raw_data_and_close_tag.split("<", 2)
      return if raw_data.empty?

      attributes = parse_attributes(raw_attributes)
      return unless attributes["translatable"] == "yes"

      data = CGI.unescapeHTML(raw_data)
      context = attributes["context"]
      if context
        po_entry = POEntry.new(:msgctxt)
        po_entry.msgctxt = context
      else
        po_entry = POEntry.new(:normal)
      end
      po_entry.msgid = data
      po_entry.references << "#{@path}:#{line_no}"
      po << po_entry
    end

    def parse_attributes(raw_attributes)
      scanner = StringScanner.new(raw_attributes)
      attributes = {}
      loop do
        scanner.scan(/\s*/m)
        break if scanner.eos?
        name = scanner.scan(/[^=]+/)
        break if name.nil?
        break unless scanner.scan(/=/)
        quote = scanner.scan(/["']/)
        break if quote.nil?
        value = scanner.scan(/[^#{Regexp.escape(quote)}]+/m)
        break if value.nil?
        break unless scanner.scan(/#{Regexp.escape(quote)}/)
        attributes[name] = CGI.unescapeHTML(value)
      end
      attributes
    end
  end
end
