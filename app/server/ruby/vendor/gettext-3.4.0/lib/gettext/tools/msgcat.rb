# Copyright (C) 2014-2017  Kouhei Sutou <kou@clear-code.com>
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

require "optparse"
require "gettext"
require "gettext/po_parser"
require "gettext/po"

module GetText
  module Tools
    class MsgCat
      class << self
        # (see #run)
        #
        # This method is provided just for convenience. It equals to
        # `new.run(*command_line)`.
        def run(*command_line)
          new.run(*command_line)
        end
      end

      # Concatenates po-files.
      #
      # @param [Array<String>] command_line
      #   The command line arguments for rmsgcat.
      # @return [void]
      def run(*command_line)
        config = Config.new
        config.parse(command_line)

        parser = POParser.new
        parser.report_warning = config.report_warning?
        parser.ignore_fuzzy = !config.include_fuzzy?
        output_po = PO.new
        output_po.order = config.order
        merger = Merger.new(output_po, config)
        config.pos.each do |po_file_name|
          po = PO.new
          parser.parse_file(po_file_name, po)
          merger.merge(po)
        end

        output_po_string = output_po.to_s(config.po_format_options)
        if config.output.is_a?(String)
          File.open(File.expand_path(config.output), "w") do |file|
            file.print(output_po_string)
          end
        else
          puts(output_po_string)
        end
      end

      # @private
      class Merger
        def initialize(output_po, config)
          @output_po = output_po
          @config = config
        end

        def merge(po)
          po.each do |entry|
            if entry.msgid == :last
              next unless @config.output_obsolete_entries?
            end
            id = [entry.msgctxt, entry.msgid]
            if @output_po.has_key?(*id)
              merged_entry = merge_entry(@output_po[*id], entry)
            else
              merged_entry = entry
            end
            next unless merged_entry
            if merged_entry.header?
              update_po_revision_date!(merged_entry)
              remove_header_fields!(merged_entry)
            end
            @output_po[*id] = merged_entry
          end
        end

        private
        def merge_entry(base_entry, new_entry)
          if base_entry.header?
            return merge_header(base_entry, new_entry)
          end

          if base_entry.fuzzy?
            return merge_fuzzy_entry(base_entry, new_entry)
          end

          if base_entry.translated?
            base_entry
          else
            new_entry
          end
        end

        def merge_header(base_entry, new_entry)
          base_entry
        end

        def merge_fuzzy_entry(base_entry, new_entry)
          return new_entry unless new_entry.fuzzy?
          return nil unless @config.include_fuzzy?
          base_entry
        end

        def update_po_revision_date!(header_entry)
          return unless @config.update_po_revision_date?

          now = Time.now.strftime("%Y-%m-%d %H:%M%z")
          po_revision_date_value = "PO-Revision-Date: #{now}\n"
          have_po_revision_date = false
          new_msgstr = String.new
          header_entry.msgstr.each_line do |line|
            case line
            when /\APO-Revision-Date:/
              new_msgstr << po_revision_date_value
              have_po_revision_date = true
            else
              new_msgstr << line
            end
          end
          unless have_po_revision_date
            new_msgstr << po_revision_date_value
          end
          header_entry.msgstr = new_msgstr
        end

        def remove_header_fields!(header_entry)
          remove_header_fields = @config.remove_header_fields
          return if remove_header_fields.empty?
          msgstr = header_entry.msgstr
          return if msgstr.nil?

          new_msgstr = String.new
          msgstr.each_line do |line|
            case line
            when /\A([\w\-]+):/
              name = $1
              next if remove_header_fields.include?(name)
            end
            new_msgstr << line
          end
          header_entry.msgstr = new_msgstr
        end
      end

      # @private
      class Config
        include GetText

        bindtextdomain("gettext")

        # @return [Array<String>] The input PO file names.
        attr_accessor :pos

        # @return [String] The output file name.
        attr_accessor :output

        # @return [:reference, :msgid] The sort key.
        attr_accessor :order

        # @return [Hash] The PO format options.
        # @see PO#to_s
        # @see POEntry#to_s
        attr_accessor :po_format_options

        # (see include_fuzzy?)
        attr_writer :include_fuzzy

        # (see report_warning?)
        attr_writer :report_warning

        # (see output_obsolete_entries?)
        attr_writer :output_obsolete_entries

        # @see #update_po_revision_date?
        attr_writer :update_po_revision_date

        # @return [Array<String>] The field names to be removed from
        #   header entry.
        attr_reader :remove_header_fields

        def initialize
          @pos = []
          @output = nil
          @order = nil
          @po_format_options = {
            :max_line_width => POEntry::Formatter::DEFAULT_MAX_LINE_WIDTH,
          }
          @include_fuzzy = true
          @report_warning = true
          @output_obsolete_entries = true
          @remove_header_fields = []
          @update_po_revision_date = false
        end

        # @return [Boolean] Whether includes fuzzy entries or not.
        def include_fuzzy?
          @include_fuzzy
        end

        # @return [Boolean] Whether reports warning messages or not.
        def report_warning?
          @report_warning
        end

        # @return [Boolean] Whether outputs obsolete entries or not.
        def output_obsolete_entries?
          @output_obsolete_entries
        end

        # @return [Boolean] Whether updates PO-Revision-Date header
        #   field or not.
        def update_po_revision_date?
          @update_po_revision_date
        end

        def parse(command_line)
          parser = create_option_parser
          @pos = parser.parse(command_line)
        end

        private
        def create_option_parser
          parser = OptionParser.new
          parser.version = GetText::VERSION
          parser.banner = _("Usage: %s [OPTIONS] PO_FILE1 PO_FILE2 ...") % $0
          parser.separator("")
          parser.separator(_("Concatenates and merges PO files."))
          parser.separator("")
          parser.separator(_("Specific options:"))

          parser.on("-o", "--output=FILE",
                    _("Write output to specified file"),
                    _("(default: the standard output)")) do |output|
            @output = output
          end

          parser.on("--sort-by-msgid",
                    _("Sort output by msgid")) do
            @order = :msgid
          end

          parser.on("--sort-by-location",
                    _("Sort output by location")) do
            @order = :reference
          end

          parser.on("--sort-by-file",
                    _("Sort output by location"),
                    _("It is same as --sort-by-location"),
                    _("Just for GNU gettext's msgcat compatibility")) do
            @order = :reference
          end

          parser.on("--[no-]sort-output",
                    _("Sort output by msgid"),
                    _("It is same as --sort-by-msgid"),
                    _("Just for GNU gettext's msgcat compatibility")) do |sort|
            @order = sort ? :msgid : nil
          end

          parser.on("--no-location",
                    _("Remove location information")) do |boolean|
            @po_format_options[:include_reference_comment] = boolean
          end

          parser.on("--no-translator-comment",
                    _("Remove translator comment")) do |boolean|
            @po_format_options[:include_translator_comment] = boolean
          end

          parser.on("--no-extracted-comment",
                    _("Remove extracted comment")) do |boolean|
            @po_format_options[:include_extracted_comment] = boolean
          end

          parser.on("--no-flag-comment",
                    _("Remove flag comment")) do |boolean|
            @po_format_options[:include_flag_comment] = boolean
          end

          parser.on("--no-previous-comment",
                    _("Remove previous comment")) do |boolean|
            @po_format_options[:include_previous_comment] = boolean
          end

          parser.on("--no-all-comments",
                    _("Remove all comments")) do |boolean|
            @po_format_options[:include_all_comments] = boolean
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

          parser.on("--no-fuzzy",
                    _("Ignore fuzzy entries")) do |include_fuzzy|
            @include_fuzzy = include_fuzzy
          end

          parser.on("--no-report-warning",
                    _("Don't report warning messages")) do |report_warning|
            @report_warning = report_warning
          end

          parser.on("--no-obsolete-entries",
                    _("Don't output obsolete entries")) do
            @output_obsolete_entries = false
          end

          parser.on("--[no-]update-po-revision-date",
                    _("Update PO-Revision-Date header field")) do |update|
            @update_po_revision_date = update
          end

          parser.on("--remove-header-field=FIELD",
                    _("Remove FIELD from header"),
                    _("Specify this option multiple times to remove multiple header fields")) do |field|
            @remove_header_fields << field
          end

          parser
        end
      end
    end
  end
end
