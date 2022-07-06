# -*- coding: utf-8 -*-
#
# Copyright (C) 2012  Haruka Yoshihara <yoshihara@clear-code.com>
# Copyright (C) 2012-2014  Kouhei Sutou <kou@clear-code.com>
# Copyright (C) 2003-2010  Masao Mutoh
# Copyright (C) 2001,2002  Yasushi Shoji, Masao Mutoh
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

require "pathname"
require "optparse"
require "locale"
require "gettext"
require "gettext/po"

module GetText
  module Tools
    class XGetText
      class << self
        def run(*arguments)
          new.run(*arguments)
        end

        # Adds a parser to the default parser list.
        #
        # @param (see #add_parser)
        # @return [void]
        #
        # @see #add_parser
        def add_parser(parser)
          @@default_parsers.unshift(parser)
        end
      end

      include GetText

      bindtextdomain("gettext")

      # @api private
      @@default_parsers = []
      builtin_parser_info_list = [
        ["ruby", "RubyParser"], # Default parser.
        ["erb", "ErbParser"],
        ["gtk_builder_ui_definitions", "GtkBuilderUIDefinitionsParser"],
        ["glade", "GladeParser"],
      ]
      builtin_parser_info_list.each do |f, klass|
        begin
          require "gettext/tools/parser/#{f}"
          add_parser(GetText.const_get(klass))
        rescue
          $stderr.puts(_("'%{klass}' is ignored.") % {:klass => klass})
          $stderr.puts($!) if $DEBUG
        end
      end

      # @return [Hash<Symbol, Object>] Options for parsing. Options
      #   are depend on each parser.
      # @see RubyParser#parse
      # @see ErbParser#parse
      attr_reader :parse_options

      def initialize #:nodoc:
        @parsers = []

        @input_files = nil
        @output = nil

        @package_name = "PACKAGE"
        @package_version = "VERSION"
        @msgid_bugs_address = ""
        @copyright_holder = "THE PACKAGE'S COPYRIGHT HOLDER"
        @copyright_year = "YEAR"
        @output_encoding = "UTF-8"

        @parse_options = {}

        @po_order = :references
        @po_format_options = {
          :max_line_width => POEntry::Formatter::DEFAULT_MAX_LINE_WIDTH,
        }
      end

      # The parser object requires to have target?(path) and
      # parse(path) method.
      #
      # @example How to add your parser
      #   require "gettext/tools/xgettext"
      #   class FooParser
      #     def target?(path)
      #       File.extname(path) == ".foo"  # *.foo file only.
      #     end
      #     def parse(path, options={})
      #       po = []
      #       # Simple entry
      #       entry = POEntry.new(:normal)
      #       entry.msgid = "hello"
      #       entry.references = ["foo.rb:200", "bar.rb:300"]
      #       entry.add_comment("Comment for the entry")
      #       po << entry
      #       # Plural entry
      #       entry = POEntry.new(:plural)
      #       entry.msgid = "An apple"
      #       entry.msgid_plural = "Apples"
      #       entry.references = ["foo.rb:200", "bar.rb:300"]
      #       po << entry
      #       # Simple entry with the entry context
      #       entry = POEntry.new(:msgctxt)
      #       entry.msgctxt = "context"
      #       entry.msgid = "hello"
      #       entry.references = ["foo.rb:200", "bar.rb:300"]
      #       po << entry
      #       # Plural entry with the message context.
      #       entry = POEntry.new(:msgctxt_plural)
      #       entry.msgctxt = "context"
      #       entry.msgid = "An apple"
      #       entry.msgid_plural = "Apples"
      #       entry.references = ["foo.rb:200", "bar.rb:300"]
      #       po << entry
      #       return po
      #     end
      #   end
      #
      #   GetText::Tools::XGetText.add_parser(FooParser.new)
      #
      # @param [#target?, #parse] parser
      #   It parses target file and extracts translate target entries from the
      #   target file. If there are multiple target files, parser.parse is
      #   called multiple times.
      # @return [void]
      def add_parser(parser)
        @parsers.unshift(parser)
      end

      def run(*options)  # :nodoc:
        check_command_line_options(*options)

        pot = generate_pot(@input_files)

        if @output.is_a?(String)
          File.open(File.expand_path(@output), "w+") do |file|
            file.puts(pot)
          end
        else
          @output.puts(pot)
        end
        self
      end

      def parse(paths) # :nodoc:
        po = PO.new
        paths = [paths] if paths.kind_of?(String)
        paths.each do |path|
          begin
            parse_path(path, po)
          rescue
            puts(_("Error parsing %{path}") % {:path => path})
            raise
          end
        end
        po
      end

      private
      def now
        Time.now
      end

      def header_comment
        <<-COMMENT
SOME DESCRIPTIVE TITLE.
Copyright (C) #{@copyright_year} #{@copyright_holder}
This file is distributed under the same license as the #{@package_name} package.
FIRST AUTHOR <EMAIL@ADDRESS>, #{@copyright_year}.

       COMMENT
      end

      def header_content
        time = now.strftime("%Y-%m-%d %H:%M%z")

        <<-CONTENT
Project-Id-Version: #{@package_name} #{@package_version}
Report-Msgid-Bugs-To: #{@msgid_bugs_address}
POT-Creation-Date: #{time}
PO-Revision-Date: #{time}
Last-Translator: FULL NAME <EMAIL@ADDRESS>
Language-Team: LANGUAGE <LL@li.org>
Language: 
MIME-Version: 1.0
Content-Type: text/plain; charset=#{@output_encoding}
Content-Transfer-Encoding: 8bit
Plural-Forms: nplurals=INTEGER; plural=EXPRESSION;
        CONTENT
      end

      def generate_pot(paths) # :nodoc:
        header = POEntry.new(:normal)
        header.msgid = ""
        header.msgstr = header_content
        header.translator_comment = header_comment
        header.flags << "fuzzy"

        po = parse(paths)
        po.order = @po_order
        po[header.msgid] = header

        to_s_options = @po_format_options.merge(:encoding => @output_encoding)
        po.to_s(to_s_options)
      end

      def check_command_line_options(*options) # :nodoc:
        input_files, output = parse_arguments(*options)

        if input_files.empty?
          raise ArgumentError, _("no input files")
        end

        output ||= STDOUT

        @input_files = input_files
        @output = output
      end

      def parse_arguments(*options) #:nodoc:
        output = nil

        parser = OptionParser.new
        banner = _("Usage: %s input.rb [-r parser.rb] [-o output.pot]") % $0
        parser.banner = banner
        parser.separator("")
        description = _("Extract translatable strings from given input files.")
        parser.separator(description)
        parser.separator("")
        parser.separator(_("Specific options:"))

        parser.on("-o", "--output=FILE",
                  _("write output to specified file")) do |out|
          output = out
        end

        parser.on("--package-name=NAME",
                  _("set package name in output"),
                  "(#{@package_name})") do |name|
          @package_name = name
        end

        parser.on("--package-version=VERSION",
                  _("set package version in output"),
                  "(#{@package_version})") do |version|
          @package_version = version
        end

        parser.on("--msgid-bugs-address=EMAIL",
                  _("set report e-mail address for msgid bugs"),
                  "(#{@msgid_bugs_address})") do |address|
          @msgid_bugs_address = address
        end

        parser.on("--copyright-holder=HOLDER",
                  _("set copyright holder in output"),
                  "(#{@copyright_holder})") do |holder|
          @copyright_holder = holder
        end

        parser.on("--copyright-year=YEAR",
                  _("set copyright year in output"),
                  "(#{@copyright_year})") do |year|
          @copyright_year = year
        end

        parser.on("--output-encoding=ENCODING",
                  _("set encoding for output"),
                  "(#{@output_encoding})") do |encoding|
          @output_encoding = encoding
        end

        parser.on("--[no-]sort-output",
                  _("Generate sorted output")) do |sort|
          @po_order = sort ? :references : nil
        end

        parser.on("--[no-]sort-by-file",
                  _("Sort output by file location")) do |sort_by_file|
          @po_order = sort_by_file ? :references : :msgid
        end

        parser.on("--[no-]sort-by-msgid",
                  _("Sort output by msgid")) do |sort_by_msgid|
          @po_order = sort_by_msgid ? :msgid : :references
        end

        parser.on("--[no-]location",
                  _("Preserve '#: FILENAME:LINE' lines")) do |location|
          @po_format_options[:include_reference_comment] = location
        end

        parser.on("--width=WIDTH", Integer,
                  _("Set output page width"),
                  "(#{@po_format_options[:max_line_width]})") do |width|
          @po_format_options[:max_line_width] = width
        end

        parser.on("--[no-]wrap",
                  _("Break long message lines, longer than the output page width, into several lines"),
                  "(#{@po_format_options[:max_line_width] >= 0})") do |wrap|
          if wrap
            max_line_width = POEntry::Formatter::DEFAULT_MAX_LINE_WIDTH
          else
            max_line_width = -1
          end
          @po_format_options[:max_line_width] = max_line_width
        end

        parser.on("-r", "--require=library",
                  _("require the library before executing xgettext")) do |out|
          require out
        end

        parser.on("-c", "--add-comments[=TAG]",
                  _("If TAG is specified, place comment blocks starting with TAG and precedding keyword lines in output file"),
                  _("If TAG is not specified, place all comment blocks preceing keyword lines in output file"),
                  _("(default: %s)") % _("no TAG")) do |tag|
          @parse_options[:comment_tag] = tag
        end

        parser.on("-d", "--debug", _("run in debugging mode")) do
          $DEBUG = true
        end

        parser.on("-h", "--help", _("display this help and exit")) do
          puts(parser.help)
          exit(true)
        end

        parser.on_tail("--version", _("display version information and exit")) do
          puts(GetText::VERSION)
          exit(true)
        end

        parser.parse!(options)

        [options, output]
      end

      def parse_path(path, po)
        (@parsers + @@default_parsers).each do |parser|
          next unless parser.target?(path)

          # For backward compatibility
          if parser.method(:parse).arity == 1 or @parse_options.empty?
            extracted_po = parser.parse(path)
          else
            extracted_po = parser.parse(path, @parse_options)
          end
          extracted_po.each do |po_entry|
            if po_entry.kind_of?(Array)
              po_entry = create_po_entry(*po_entry)
            end

            if po_entry.msgid.empty?
              warn _("Warning: The empty \"\" msgid is reserved by " +
                       "gettext. So gettext(\"\") doesn't returns " +
                       "empty string but the header entry in po file.")
              # TODO: add pommesage.reference to the pot header as below:
              # # SOME DESCRIPTIVE TITLE.
              # # Copyright (C) YEAR THE COPYRIGHT HOLDER
              # # This file is distributed under the same license as the PACKAGE package.
              # # FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
              # #
              # #: test/test_gettext.rb:65
              # #, fuzzy
              # "#: test/test_gettext.rb:65" line is added.
              next
            end

            if @output.is_a?(String)
              base_path = Pathname.new(@output).dirname.expand_path
              po_entry.references = po_entry.references.collect do |reference|
                path, line, = reference.split(/:(\d+)\z/, 2)
                absolute_path = Pathname.new(path).expand_path
                begin
                  path = absolute_path.relative_path_from(base_path).to_s
                rescue ArgumentError
                  raise # Should we ignore it?
                end
                "#{path}:#{line}"
              end
            end

            existing_entry = po[po_entry.msgctxt, po_entry.msgid]
            if existing_entry
              po_entry = existing_entry.merge(po_entry)
            end
            po[po_entry.msgctxt, po_entry.msgid] = po_entry
          end
          break
        end
      end

      def create_po_entry(msgid, *references)
        type = :normal
        msgctxt = nil
        msgid_plural = nil

        if msgid.include?("\004")
          msgctxt, msgid = msgid.split(/\004/, 2)
          type = :msgctxt
        end
        if msgid.include?("\000")
          msgid, msgid_plural = msgid.split(/\000/, 2)
          if type == :msgctxt
            type = :msgctxt_plural
          else
            type = :plural
          end
        end

        po_entry = POEntry.new(type)
        po_entry.msgid = msgid
        po_entry.msgctxt = msgctxt
        po_entry.msgid_plural = msgid_plural
        po_entry.references = references
        po_entry
      end
    end
  end
end
