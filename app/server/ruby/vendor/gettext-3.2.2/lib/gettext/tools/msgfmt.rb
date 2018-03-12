# -*- coding: utf-8 -*-
#
# Copyright (C) 2012  Kouhei Sutou <kou@clear-code.com>
# Copyright (C) 2012  Haruka Yoshihara <yoshihara@clear-code.com>
# Copyright (C) 2003-2009 Masao Mutoh
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
require "fileutils"
require "gettext"
require "gettext/po_parser"

module GetText
  module Tools
    class MsgFmt  #:nodoc:
      # Create a mo-file from a target file(po-file).
      # You must specify a path of a target file in arguments.
      # If a path of a mo-file is not specified in arguments, a mo-file is
      # created as "messages.mo" in the current directory.
      # @param [Array<String>] arguments arguments for rmsgfmt.
      # @return [void]
      class << self
        def run(*arguments)
          new.run(*arguments)
        end
      end

      include GetText

      bindtextdomain("gettext")

      def initialize
        @input_file = nil
        @output_file = nil
      end

      def run(*options) # :nodoc:
        initialize_arguments(*options)

        parser = POParser.new
        data = MO.new

        parser.parse_file(@input_file, data)
        data.save_to_file(@output_file)
      end

      def initialize_arguments(*options) # :nodoc:
        input_file, output_file = parse_commandline_options(*options)

        if input_file.nil?
          raise(ArgumentError, _("no input files specified."))
        end

        if output_file.nil?
          output_file = "messages.mo"
        end

        @input_file = input_file
        @output_file = output_file
      end

      def parse_commandline_options(*options)
        output_file = nil

        parser = OptionParser.new
        parser.banner = _("Usage: %s input.po [-o output.mo]" % $0)
        parser.separator("")
        description = _("Generate binary message catalog from textual " +
                          "translation description.")
        parser.separator(description)
        parser.separator("")
        parser.separator(_("Specific options:"))

        parser.on("-o", "--output=FILE",
                _("write output to specified file")) do |out|
          output_file = out
        end

        parser.on_tail("--version", _("display version information and exit")) do
          puts(VERSION)
          exit(true)
        end
        parser.parse!(options)

        input_file = options[0]
        [input_file, output_file]
      end
    end
  end
end
